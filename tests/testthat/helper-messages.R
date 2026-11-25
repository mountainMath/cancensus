# Messages follow the system locale when the cancensus.language option is not set. Tests match
# English message texts, so run them with an English locale independent of the machine.
Sys.setenv(LANGUAGE = "en")

# Messages from other packages can show up only on some check machines, like duckdb's notice about
# disabled extensions on Linux builds compiled with libc++ (CRAN's fedora-clang). On CRAN, where we
# can't follow up on these, expect_no_message() and capture_messages() only consider messages from
# cancensus, which all have the class "cancensus_message", and let other messages pass through. Locally
# and on CI they consider all messages, so that new messages from other packages get noticed.
only_cancensus_messages <- function() !identical(Sys.getenv("NOT_CRAN"), "true")

expect_no_message <- function(object, ..., class = NULL) {
  if (is.null(class) && only_cancensus_messages()) class <- "cancensus_message"
  rlang::inject(testthat::expect_no_message(!!rlang::enquo(object), ..., class = class))
}

capture_messages <- function(code) {
  if (!only_cancensus_messages()) return(testthat::capture_messages(code))
  messages <- character()
  withCallingHandlers(code, cancensus_message = function(m) {
    messages <<- c(messages, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  messages
}
