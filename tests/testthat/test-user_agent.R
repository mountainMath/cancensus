# Evaluate code with the cancensus.user_agent_prefix option temporarily set
with_prefix <- function(prefix, code) {
  old <- options(cancensus.user_agent_prefix = prefix)
  on.exit(options(old))
  force(code)
}
base_user_agent <- function() with_prefix(NULL, cancensus_user_agent())

test_that("user agent leads with package name and version", {
  ua <- base_user_agent()
  expect_match(ua, "^cancensus/\\d+\\.\\d+\\.\\d+(\\.\\d+)? httr/[0-9.]+ R/[0-9.]+$")
  expect_true(startsWith(ua, paste0("cancensus/", utils::packageVersion("cancensus"))))
})

test_that("user agent prefix option is prepended", {
  expect_equal(with_prefix("mypackage/1.2.3", cancensus_user_agent()),
               paste("mypackage/1.2.3", base_user_agent()))
})

test_that("invalid or empty user agent prefixes are ignored or sanitized", {
  for (prefix in list("", "   ", NA_character_, 42, c("a/1", "b/2"))) {
    expect_equal(with_prefix(prefix, cancensus_user_agent()), base_user_agent())
  }

  # Control characters must not make it into the header
  ua <- with_prefix(" mypackage/1.2.3\r\nX-Injected: 1 ", cancensus_user_agent())
  expect_false(grepl("[[:cntrl:]]", ua))
  expect_equal(ua, paste("mypackage/1.2.3 X-Injected: 1", base_user_agent()))
})

test_that("request config carries the user agent and optional progress", {
  quiet_config <- cm_request_config(quiet = TRUE)
  expect_s3_class(quiet_config, "request")
  expect_equal(quiet_config$options$useragent, cancensus_user_agent())
  expect_null(quiet_config$options$noprogress)

  loud_config <- cm_request_config(quiet = FALSE)
  expect_equal(loud_config$options$useragent, cancensus_user_agent())
  expect_false(loud_config$options$noprogress)
})

test_that("cm_GET and cm_POST send the user agent with the request", {
  captured <- list()
  local_mocked_bindings(
    GET = function(url, config = list(), ...) {
      captured$get <<- list(url = url, config = c(config, ...))
      "get-response"
    },
    POST = function(url, config = list(), ..., body = NULL) {
      captured$post <<- list(url = url, config = c(config, ...), body = body)
      "post-response"
    },
    .package = "httr"
  )

  with_prefix("wrapper/0.1", {
    expect_equal(cm_GET("https://example.com/a", httr::accept_json()), "get-response")
    expect_equal(captured$get$url, "https://example.com/a")
    expect_equal(captured$get$config$options$useragent, cancensus_user_agent())
    expect_match(captured$get$config$options$useragent, "^wrapper/0\\.1 cancensus/")
    expect_equal(unname(captured$get$config$headers[["Accept"]]), "application/json")

    expect_equal(cm_POST("https://example.com/b", body = list(x = 1)), "post-response")
    expect_equal(captured$post$url, "https://example.com/b")
    expect_equal(captured$post$body, list(x = 1))
    expect_equal(captured$post$config$options$useragent, cancensus_user_agent())
  })
})
