# Tests for session management (session.R)

# Helper: snapshot session state and restore on exit
local_clean_session <- function(env = parent.frame()) {
  tezr_env <- get("tezr_env", envir = asNamespace("tezr"))

  old_last_request <- tezr_env$last_request
  old_cookies <- tezr_env$cookies
  old_request_count <- tezr_env$request_count
  old_session_start <- tezr_env$session_start
  old_has_last_search_mode <- exists(
    "last_search_mode",
    envir = tezr_env,
    inherits = FALSE
  )
  old_last_search_mode <- tezr_env$last_search_mode

  withr::defer(
    {
      tezr_env$last_request <- old_last_request
      tezr_env$cookies <- old_cookies
      tezr_env$request_count <- old_request_count
      tezr_env$session_start <- old_session_start
      if (old_has_last_search_mode) {
        tezr_env$last_search_mode <- old_last_search_mode
      } else if (
        exists("last_search_mode", envir = tezr_env, inherits = FALSE)
      ) {
        rm(list = "last_search_mode", envir = tezr_env)
      }
    },
    envir = env
  )
}

test_that("has_session works", {
  has_session_flag <- has_session()
  expect_type(has_session_flag, "logical")
})

test_that("default_request_headers includes required fields", {
  headers <- default_request_headers()
  expect_true("User-Agent" %in% names(headers))
  expect_true("Accept-Language" %in% names(headers))
  expect_true("Content-Type" %in% names(headers))
})

test_that("create_session applies default headers", {
  local_clean_session()
  called <- FALSE
  orig <- default_request_headers

  testthat::local_mocked_bindings(
    default_request_headers = function() {
      called <<- TRUE
      orig()
    },
    .package = "tezr"
  )

  create_session()
  expect_true(called)
})

test_that("create_session applies rate limit by default", {
  local_clean_session()
  rate_limit_called <- FALSE

  testthat::local_mocked_bindings(
    rate_limit = function(...) {
      rate_limit_called <<- TRUE
      invisible(NULL)
    },
    .package = "tezr"
  )

  create_session()
  expect_true(rate_limit_called)
})

test_that("create_session can skip rate limiting", {
  local_clean_session()
  rate_limit_called <- FALSE

  testthat::local_mocked_bindings(
    rate_limit = function(...) {
      rate_limit_called <<- TRUE
      invisible(NULL)
    },
    .package = "tezr"
  )

  create_session(apply_rate_limit = FALSE)
  expect_false(rate_limit_called)
})

test_that("init_session applies default headers", {
  local_clean_session()
  called <- FALSE
  orig <- default_request_headers

  testthat::local_mocked_bindings(
    default_request_headers = function() {
      called <<- TRUE
      orig()
    },
    .package = "tezr"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      structure(list(), class = "httr2_response")
    },
    resp_header = function(...) NULL,
    .package = "httr2"
  )

  init_session()

  expect_true(called)
})

test_that("init_session applies retry policy", {
  local_clean_session()
  retry_called <- FALSE

  testthat::local_mocked_bindings(
    req_retry = function(req, ...) {
      retry_called <<- TRUE
      req
    },
    req_perform = function(req, ...) {
      structure(list(), class = "httr2_response")
    },
    resp_header = function(...) NULL,
    .package = "httr2"
  )

  init_session()

  expect_true(retry_called)
})

test_that("rate_limit sleeps the correct remaining time", {
  local_clean_session()

  sleep_value <- NULL
  testthat::local_mocked_bindings(
    Sys.sleep = function(x) sleep_value <<- x,
    .package = "base"
  )

  tezr_env <- get("tezr_env", envir = asNamespace("tezr"))

  # Set last request to 0.5 seconds ago
  tezr_env$last_request <- Sys.time() - 0.5

  rate_limit(delay = 2)

  # Should have slept ~1.5 seconds (2 - 0.5)
  expect_true(!is.null(sleep_value))
  expect_true(sleep_value > 1.0 && sleep_value <= 2.0)
})

test_that("rate_limit does not sleep when enough time elapsed", {
  local_clean_session()

  sleep_called <- FALSE
  testthat::local_mocked_bindings(
    Sys.sleep = function(x) sleep_called <<- TRUE,
    .package = "base"
  )

  tezr_env <- get("tezr_env", envir = asNamespace("tezr"))

  # Set last request to 5 seconds ago
  tezr_env$last_request <- Sys.time() - 5

  rate_limit(delay = 2)

  expect_false(sleep_called)
})

test_that("rate_limit does not sleep on first call", {
  local_clean_session()

  sleep_called <- FALSE
  testthat::local_mocked_bindings(
    Sys.sleep = function(x) sleep_called <<- TRUE,
    .package = "base"
  )

  tezr_env <- get("tezr_env", envir = asNamespace("tezr"))

  # No previous timestamp
  tezr_env$last_request <- NULL

  rate_limit(delay = 2)

  expect_false(sleep_called)
})

test_that("init_session stores cookies from response header", {
  local_clean_session()

  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      structure(list(), class = "httr2_response")
    },
    resp_header = function(resp, name, ...) "JSESSIONID=abc123; Path=/",
    .package = "httr2"
  )

  tezr_env <- get("tezr_env", envir = asNamespace("tezr"))
  init_session()

  expect_equal(tezr_env$cookies, "JSESSIONID=abc123")
})

test_that("init_session handles missing Set-Cookie header", {
  local_clean_session()

  tezr_env <- get("tezr_env", envir = asNamespace("tezr"))
  tezr_env$cookies <- NULL

  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      structure(list(), class = "httr2_response")
    },
    resp_header = function(resp, name, ...) NULL,
    .package = "httr2"
  )

  init_session()

  # Cookies should remain NULL when no Set-Cookie header
  expect_null(tezr_env$cookies)
})

test_that("init_session clears previous search mode", {
  local_clean_session()

  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      structure(list(), class = "httr2_response")
    },
    resp_header = function(...) NULL,
    .package = "httr2"
  )

  tezr_env <- get("tezr_env", envir = asNamespace("tezr"))
  tezr_env$last_search_mode <- "4"

  init_session()

  expect_false(exists("last_search_mode", envir = tezr_env, inherits = FALSE))
})

test_that("has_session returns FALSE before init, TRUE after", {
  local_clean_session()

  tezr_env <- get("tezr_env", envir = asNamespace("tezr"))

  tezr_env$cookies <- NULL
  expect_false(has_session())

  tezr_env$cookies <- "JSESSIONID=test"
  expect_true(has_session())
})

test_that("refresh_session_if_needed explains refresh reason", {
  local_clean_session()
  tezr_env <- get("tezr_env", envir = asNamespace("tezr"))
  tezr_env$request_count <- 50L
  tezr_env$session_start <- Sys.time()

  refresh_called <- FALSE
  info_messages <- character()

  testthat::local_mocked_bindings(
    init_session = function(...) {
      refresh_called <<- TRUE
      invisible(TRUE)
    },
    .package = "tezr"
  )
  testthat::local_mocked_bindings(
    cli_alert_info = function(text, ...) {
      info_messages <<- c(info_messages, paste(text, collapse = " "))
      invisible(NULL)
    },
    .package = "cli"
  )

  refresh_session_if_needed()

  expect_true(refresh_called)
  expect_true(any(grepl("request count", info_messages, ignore.case = TRUE)))
})
