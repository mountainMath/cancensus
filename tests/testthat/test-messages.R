# Bilingual errors, warnings and messages

catalog <- function() {
  utils::read.csv(system.file("i18n", "messages.csv", package = "cancensus"),
                  stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = character(0))
}

placeholders <- function(x) sort(unique(regmatches(x, gregexpr("\\{[a-z_]+\\}", x))[[1]]))

test_that("every message has an English and a French text with the same placeholders", {
  rows <- catalog()
  expect_false(anyDuplicated(rows$id) > 0)
  expect_true(all(nzchar(rows$en)))
  expect_true(all(nzchar(rows$fr)))
  for (i in seq_len(nrow(rows))) {
    expect_identical(placeholders(rows$fr[i]), placeholders(rows$en[i]), label = rows$id[i])
  }
})

test_that("messages with counts have a singular and a plural form", {
  ids <- catalog()$id
  counted <- unique(sub("_(one|other)$", "", grep("_(one|other)$", ids, value = TRUE)))
  for (id in counted) {
    expect_true(all(paste0(id, c("_one", "_other")) %in% ids), label = id)
  }
})

test_that("every message used in the code is in the catalog", {
  r_dir <- test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "package sources not available")
  code <- unlist(lapply(list.files(r_dir, pattern = "\\.R$", full.names = TRUE), readLines, warn = FALSE))
  calls <- regmatches(code, gregexpr("cc_(abort|warn|inform|msg)\\(\"[a-z_]+\"", code))
  used <- unique(sub("^.*\\(\"([a-z_]+)\"$", "\\1", unlist(calls)))
  ids <- catalog()$id
  counted <- unique(sub("_(one|other)$", "", grep("_(one|other)$", ids, value = TRUE)))
  expect_length(setdiff(used, c(ids, counted)), 0)
})

test_that("the message language follows the parameter, then the option, then the locale", {
  old_language <- Sys.getenv("LANGUAGE")
  on.exit(Sys.setenv(LANGUAGE = old_language))
  rlang::local_options(cancensus.language = NULL)

  Sys.setenv(LANGUAGE = "fr_CA")
  expect_equal(message_language(), "fr")
  Sys.setenv(LANGUAGE = "en")
  expect_equal(message_language(), "en")

  rlang::local_options(cancensus.language = "fr")
  expect_equal(message_language(), "fr")
  expect_equal(message_language("en"), "en")
  expect_equal(message_language("FR"), "fr")
  # invalid languages are skipped, messages never fail
  expect_equal(message_language("de"), "fr")
  rlang::local_options(cancensus.language = "de")
  expect_equal(message_language(), "en")
})

test_that("messages get filled in the requested language", {
  expect_equal(cc_msg("unknown_level", level = "XY", .language = "en"), "Unknown level XY")
  expect_equal(cc_msg("unknown_level", level = "XY", .language = "fr"), "Niveau inconnu : XY")
  expect_equal(cc_msg("invalid_language", choices = c("en", "fr"), .language = "en"),
               "Language must be one of en, fr.")
  # values are inserted literally
  expect_equal(cc_msg("unknown_level", level = "a\\1{level}", .language = "en"), "Unknown level a\\1{level}")
  expect_error(cc_msg("no_such_message"), "Unknown cancensus message id")
})

test_that("counts select the singular or plural form of the language", {
  expect_equal(cc_msg("processing_features", .n = 1, .language = "en"), "Processing 1 geographic feature.")
  expect_equal(cc_msg("processing_features", .n = 0, .language = "en"), "Processing 0 geographic features.")
  expect_equal(cc_msg("processing_features", .n = 0, .language = "fr"), "Traitement de 0 entité géographique.")
  expect_equal(cc_msg("processing_features", .n = 12345, .language = "fr"),
               "Traitement de 12 345 entités géographiques.")
  expect_equal(cc_msg("processing_features", .n = 12345, .language = "en"), "Processing 12,345 geographic features.")
})

test_that("conditions have classes independent of the language", {
  expect_error(cc_abort("unknown_level", level = "XY", .language = "fr"),
               class = "cancensus_unknown_level")
  expect_error(cc_abort("unknown_level", level = "XY"), class = "cancensus_error")
  expect_warning(cc_warn("unknown_level", level = "XY"), class = "cancensus_warning")
  expect_message(cc_inform("unknown_level", level = "XY"), class = "cancensus_message")
})

test_that("explicit language parameters set the language of errors", {
  rlang::local_options(cancensus.language = NULL)
  expect_error(validate_language("de"), "Language must be one of en, fr")
  rlang::local_options(cancensus.language = "fr")
  expect_error(validate_language("de"), "La langue doit")
  expect_error(get_census("CA21", regions = list(PR = "59"), labels = "x", language = "en"),
               "The `labels` parameter must be one of")
  expect_error(get_census("CA21", regions = list(PR = "59"), labels = "x"),
               "Le paramètre `labels`")
})

test_that("helpers with a language parameter give messages in that language", {
  rlang::local_options(cancensus.language = NULL)
  blocked <- statcan_blocked_message("https://example.com/a.zip", "/cache/statcan_downloads/a.zip", TRUE,
                                     language = "fr")
  expect_match(blocked, "Statistique Canada a bloqu", fixed = TRUE)
  expect_match(blocked, "https://example.com/a.zip", fixed = TRUE)
  expect_match(statcan_blocked_message("https://example.com/a.zip", "/cache/a.zip", TRUE), "Cloudflare browser")

  expect_error(geo_suite_table_name("XY", "2021", language = "fr"), class = "cancensus_geo_suite_no_table")
  expect_error(geo_suite_table_name("XY", "2021", language = "fr"), "ne contient pas de table XY")
  expect_error(geo_suite_table_name("DA", "1996"), "enumeration areas")

  expect_equal(cc_msg("keyword_more_not_interactive", .n = 1, .language = "en"),
               "1 additional keyword match with less precision is not shown since the session is not interactive.")
  expect_match(cc_msg("keyword_more_not_interactive", .n = 2, .language = "fr"), "^2 correspondances")
})

test_that("byte sizes are formatted for the language", {
  expect_equal(format_bytes(512, "en"), "512 B")
  expect_equal(format_bytes(1536, "en"), "1.5 KB")
  expect_equal(format_bytes(1536, "fr"), "1,5 Ko")
  expect_equal(format_bytes(3 * 1024^3, "fr"), "3,0 Go")
})

test_that("the count of a message is not confused with other values starting with n", {
  expect_equal(cc_msg("processing_features", .n = 3, name = "x", .language = "en"),
               "Processing 3 geographic features.")
  expect_equal(cc_msg("processing_features", .n = 3, n = "three", .language = "en"),
               "Processing three geographic features.")
})
