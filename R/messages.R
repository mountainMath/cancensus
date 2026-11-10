# Bilingual errors, warnings and messages.
#
# The texts live in inst/i18n/messages.csv, one row per message with columns `id`, `en` and `fr`.
# Templates refer to values with `{name}` placeholders. A message with a count has two rows,
# `<id>_one` and `<id>_other`, and gets selected via `.n`. Keeping the texts in a UTF-8 file keeps the
# R code ASCII as required by R CMD check, and lets translators work without touching code.
#
# Conditions get the class `cancensus_<id>`, so tests and users can check for specific conditions
# independent of the language of the message.

.cancensus_messages <- new.env(parent = emptyenv())

message_catalog <- function() {
  if (is.null(.cancensus_messages$catalog)) {
    path <- system.file("i18n", "messages.csv", package = "cancensus")
    rows <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8",
                            na.strings = character(0), check.names = FALSE)
    .cancensus_messages$catalog <- list(en = stats::setNames(enc2utf8(rows$en), rows$id),
                                        fr = stats::setNames(enc2utf8(rows$fr), rows$id))
  }
  .cancensus_messages$catalog
}

# The language for messages: an explicitly requested language if valid, otherwise the
# cancensus.language option, otherwise the system locale, otherwise English. Unlike validate_language
# this never fails, so that errors about invalid languages can be reported in a sensible language.
# Data labels deliberately don't follow the system locale, scripts should give the same results on
# all machines.
message_language <- function(language = NULL) {
  is_valid <- function(x) is.character(x) && length(x) == 1 && !is.na(x) && tolower(x) %in% c("en", "fr")
  if (is_valid(language)) return(tolower(language))
  option <- getOption("cancensus.language")
  if (is_valid(option)) return(tolower(option))
  locale_language()
}

# Language of the system locale, following the precedence of gettext
locale_language <- function() {
  locales <- c(Sys.getenv(c("LANGUAGE", "LC_ALL", "LC_MESSAGES", "LANG")),
               tryCatch(Sys.getlocale("LC_MESSAGES"), error = function(e) ""),
               tryCatch(Sys.getlocale("LC_CTYPE"), error = function(e) ""))
  locales <- locales[nzchar(locales) & !(locales %in% c("C", "POSIX"))]
  if (length(locales) > 0 && grepl("^(fr|french)", tolower(locales[1]))) "fr" else "en"
}

# French counts zero as singular, English does not
is_singular <- function(n, language) {
  if (language == "fr") abs(n) < 2 else n == 1
}

# The message `id` in the message language, with the `{name}` placeholders filled by the named
# arguments. Vectors get collapsed into comma separated lists.
cc_msg <- function(id, ..., .n = NULL, .language = NULL) {
  language <- message_language(.language)
  if (!is.null(.n)) id <- paste0(id, if (is_singular(.n, language)) "_one" else "_other")
  catalog <- message_catalog()
  template <- catalog[[language]][id]
  if (is.na(template) || !nzchar(template)) template <- catalog$en[id]
  if (is.na(template)) stop("Unknown cancensus message id '", id, "'.", call. = FALSE)
  values <- list(...)
  if (!is.null(.n) && is.null(values[["n"]])) values[["n"]] <- format_number(.n, language)
  for (name in names(values)) {
    value <- paste(as.character(values[[name]]), collapse = ", ")
    # split rather than gsub, to not interpret backslashes in the value
    parts <- strsplit(paste0(template, "\u0001"), paste0("{", name, "}"), fixed = TRUE)[[1]]
    template <- sub("\u0001$", "", paste(parts, collapse = value))
  }
  unname(template)
}

cc_abort <- function(id, ..., .n = NULL, .language = NULL, .parent = NULL) {
  rlang::abort(cc_msg(id, ..., .n = .n, .language = .language),
               class = c(paste0("cancensus_", id), "cancensus_error"),
               parent = .parent, call = NULL)
}

cc_warn <- function(id, ..., .n = NULL, .language = NULL) {
  rlang::warn(cc_msg(id, ..., .n = .n, .language = .language),
              class = c(paste0("cancensus_", id), "cancensus_warning"))
}

cc_inform <- function(id, ..., .n = NULL, .language = NULL) {
  rlang::inform(cc_msg(id, ..., .n = .n, .language = .language),
                class = c(paste0("cancensus_", id), "cancensus_message"))
}

# Numbers with the digit grouping of the language, a non-breaking space in French
format_number <- function(x, language = NULL) {
  language <- message_language(language)
  format(x, big.mark = if (language == "fr") "\u00a0" else ",", scientific = FALSE, trim = TRUE)
}
