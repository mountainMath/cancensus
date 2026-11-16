# Requests return the responses in `responses` in turn, the waits get recorded instead of slept
local_retry_mocks <- function(responses, env = parent.frame()) {
  state <- new.env()
  state$calls <- 0
  state$waits <- numeric(0)
  local_mocked_bindings(cc_sleep = function(seconds) state$waits <- c(state$waits, seconds),
                        .env = env)
  state$call_fn <- function() {
    state$calls <- state$calls + 1
    response <- responses[[min(state$calls, length(responses))]]
    if (inherits(response, "error")) stop(response)
    response
  }
  state
}

busy <- function() fake_response(503, list(`retry-after` = "10"))

# A response with a text body, as handle_cm_status_code reads it
with_body <- function(response, body) {
  response$content <- charToRaw(body)
  response$headers$`content-type` <- "text/plain"
  response
}

test_that("a busy server gets retried twice even without retry, honouring Retry-After", {
  state <- local_retry_mocks(list(busy(), busy(), fake_response(200)))
  expect_message(response <- retry_api_call(state$call_fn, quiet = TRUE),
                 class = "cancensus_retry_busy")
  expect_equal(httr::status_code(response), 200)
  expect_equal(state$calls, 3)
  expect_equal(state$waits, c(10, 10))
})

test_that("a server that stays busy returns the last response after the busy retries", {
  state <- local_retry_mocks(list(busy()))
  response <- suppressMessages(retry_api_call(state$call_fn))
  expect_equal(httr::status_code(response), 503)
  expect_equal(state$calls, 3)
  state <- local_retry_mocks(list(busy()))
  response <- suppressMessages(retry_api_call(state$call_fn, retries = 4))
  expect_equal(state$calls, 5)
})

test_that("Retry-After is capped at 60 seconds and short waits are quiet", {
  state <- local_retry_mocks(list(fake_response(429, list(`retry-after` = "600")), fake_response(200)))
  suppressMessages(retry_api_call(state$call_fn, quiet = TRUE))
  expect_equal(state$waits, 60)
  state <- local_retry_mocks(list(fake_response(503), fake_response(200)))
  expect_no_message(retry_api_call(state$call_fn, quiet = TRUE))
  expect_equal(state$waits, 1)
})

test_that("other transient errors only get retried when asked for", {
  state <- local_retry_mocks(list(fake_response(502), fake_response(200)))
  expect_equal(httr::status_code(retry_api_call(state$call_fn)), 502)
  expect_equal(state$calls, 1)
  state <- local_retry_mocks(list(fake_response(502), fake_response(504), fake_response(200)))
  expect_message(response <- retry_api_call(state$call_fn, retries = 2), class = "cancensus_retry_http")
  expect_equal(httr::status_code(response), 200)
  expect_equal(state$waits, c(1, 2))
})

test_that("server errors and client errors are not retried", {
  for (status in c(500, 400, 401, 404)) {
    state <- local_retry_mocks(list(fake_response(status), fake_response(200)))
    expect_equal(httr::status_code(retry_api_call(state$call_fn, retries = 3)), status)
    expect_equal(state$calls, 1)
  }
})

test_that("network errors get retried when asked for and are passed on otherwise", {
  state <- local_retry_mocks(list(simpleError("Could not resolve host")))
  expect_error(retry_api_call(state$call_fn), "Could not resolve host")
  expect_equal(state$calls, 1)
  state <- local_retry_mocks(list(simpleError("Could not resolve host"), fake_response(200)))
  expect_message(response <- retry_api_call(state$call_fn, retries = 1), class = "cancensus_retry_network")
  expect_equal(httr::status_code(response), 200)
})

test_that("API requests stop with a specific error when the server stays busy", {
  state <- local_retry_mocks(list(with_body(busy(), "Server busy, please retry shortly.")))
  local_mocked_bindings(cm_POST = function(...) state$call_fn(), cm_GET = function(...) state$call_fn())
  expect_error(suppressMessages(cm_api_request("data.csv", list(), quiet = TRUE)),
               class = "cancensus_download_failed_busy")
  expect_equal(state$calls, 3)
  expect_error(suppressMessages(list_census_datasets(use_cache = FALSE, quiet = TRUE)),
               "server is busy", class = "cancensus_download_failed_busy")
  expect_equal(state$calls, 6)
})

test_that("metadata requests get retried when the server is busy", {
  state <- local_retry_mocks(list(busy(), with_body(fake_response(500), "Internal error")))
  local_mocked_bindings(cm_GET = function(...) state$call_fn())
  expect_error(list_census_regions("CA21", use_cache = FALSE, quiet = TRUE),
               class = "cancensus_download_failed_server")
  expect_equal(state$calls, 2)
})
