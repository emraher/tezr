# Tests for session management (session.R)

# Helper: snapshot session state and restore on exit
local_clean_session <- function(env = parent.frame()) {
  tezr_env <- get("tezr_env", envir = asNamespace("tezr"))

  old_last_request <- tezr_env$last_request
  old_cookies <- tezr_env$cookies
  old_request_count <- tezr_env$request_count
  old_session_start <- tezr_env$session_start

  withr::defer(
    {
      tezr_env$last_request <- old_last_request
      tezr_env$cookies <- old_cookies
      tezr_env$request_count <- old_request_count
      tezr_env$session_start <- old_session_start
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

test_that("default user agent identifies the package", {
  withr::local_options(list(tezr.user_agent = NULL))
  withr::local_envvar(TEZR_USER_AGENT = NA)

  headers <- default_request_headers()

  expect_match(headers["User-Agent"], "^tezr/")
  expect_false(grepl("Mozilla", headers["User-Agent"], fixed = TRUE))
  expect_match(headers["User-Agent"], "github.com/emraher/tezr")
  expect_match(headers["User-Agent"], "mailto:eer@eremrah.com")
})

test_that("default user agent identifies GitHub Actions runs", {
  withr::local_options(list(tezr.user_agent = NULL))
  withr::local_envvar(
    GITHUB_ACTIONS = "true",
    TEZR_USER_AGENT = NA
  )

  expect_match(request_user_agent(), "GitHub Actions")
})

test_that("user agent can be overridden by environment and option", {
  withr::local_options(list(tezr.user_agent = NULL))
  withr::local_envvar(TEZR_USER_AGENT = "env-agent")

  expect_identical(request_user_agent(), "env-agent")

  withr::local_options(list(tezr.user_agent = "option-agent"))
  expect_identical(request_user_agent(), "option-agent")
})

test_that("request_config sets and resets request options", {
  withr::local_options(list(tezr.user_agent = NULL, tezr.verbose = NULL))
  withr::local_envvar(
    TEZR_USER_AGENT = NA,
    TEZR_VERBOSE = NA
  )

  request_config(user_agent = "custom-agent", verbose = FALSE)

  expect_identical(request_user_agent(), "custom-agent")
  expect_false(tezr_verbose())

  request_config(reset = TRUE)

  expect_match(request_user_agent(), "^tezr/")
  expect_true(tezr_verbose())
})

test_that("request_config validates request option values", {
  expect_error(request_config(user_agent = ""), "user_agent")
  expect_error(request_config(verbose = NA), "verbose")
  expect_error(request_config(reset = NA), "reset")
})

test_that("verbosity can be controlled by option and environment variable", {
  withr::local_options(list(tezr.verbose = NULL))
  withr::local_envvar(TEZR_VERBOSE = "false")
  expect_false(tezr_verbose())

  withr::local_options(list(tezr.verbose = TRUE))
  expect_true(tezr_verbose())
})

test_that("tezr_verbose rejects invalid option values", {
  withr::local_options(list(tezr.verbose = "yes"))

  expect_error(tezr_verbose(), "tezr.verbose")
})

test_that("environment logical parser handles true and fallback values", {
  expect_true(parse_env_logical("yes", FALSE))
  expect_false(parse_env_logical("off", TRUE))
  expect_true(parse_env_logical("not-a-bool", TRUE))
  expect_false(parse_env_logical("", FALSE))
})

test_that("informational messages respect verbosity", {
  withr::local_options(list(tezr.verbose = FALSE))

  state <- new.env(parent = emptyenv())
  state$info_called <- FALSE
  state$success_called <- FALSE
  testthat::local_mocked_bindings(
    cli_alert_info = function(...) {
      state$info_called <- TRUE
      invisible(NULL)
    },
    cli_alert_success = function(...) {
      state$success_called <- TRUE
      invisible(NULL)
    },
    .package = "cli"
  )

  tezr_inform("hidden")
  tezr_success("hidden")

  expect_false(state$info_called)
  expect_false(state$success_called)
})

test_that("create_session applies default headers", {
  local_clean_session()
  state <- new.env(parent = emptyenv())
  state$called <- FALSE
  orig <- default_request_headers

  testthat::local_mocked_bindings(
    default_request_headers = function() {
      state$called <- TRUE
      orig()
    },
    .package = "tezr"
  )

  create_session()
  expect_true(state$called)
})

test_that("create_session applies rate limit by default", {
  local_clean_session()
  state <- new.env(parent = emptyenv())
  state$rate_limit_called <- FALSE

  testthat::local_mocked_bindings(
    rate_limit = function(...) {
      state$rate_limit_called <- TRUE
      invisible(NULL)
    },
    .package = "tezr"
  )

  create_session()
  expect_true(state$rate_limit_called)
})

test_that("create_session can skip rate limiting", {
  local_clean_session()
  state <- new.env(parent = emptyenv())
  state$rate_limit_called <- FALSE

  testthat::local_mocked_bindings(
    rate_limit = function(...) {
      state$rate_limit_called <- TRUE
      invisible(NULL)
    },
    .package = "tezr"
  )

  create_session(apply_rate_limit = FALSE)
  expect_false(state$rate_limit_called)
})

test_that("init_session applies default headers", {
  local_clean_session()
  state <- new.env(parent = emptyenv())
  state$called <- FALSE
  orig <- default_request_headers

  testthat::local_mocked_bindings(
    default_request_headers = function() {
      state$called <- TRUE
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

  expect_true(state$called)
})

test_that("rate_limit sleeps the correct remaining time", {
  local_clean_session()

  state <- new.env(parent = emptyenv())
  state$sleep_value <- NULL
  testthat::local_mocked_bindings(
    Sys.sleep = function(x) state$sleep_value <- x,
    .package = "base"
  )

  tezr_env <- get("tezr_env", envir = asNamespace("tezr"))

  # Set last request to 0.5 seconds ago
  tezr_env$last_request <- Sys.time() - 0.5

  rate_limit(delay = 2)

  # Should have slept ~1.5 seconds (2 - 0.5)
  expect_false(is.null(state$sleep_value))
  expect_gt(state$sleep_value, 1.0)
  expect_lte(state$sleep_value, 2.0)
})

test_that("rate_limit does not sleep when enough time elapsed", {
  local_clean_session()

  state <- new.env(parent = emptyenv())
  state$sleep_called <- FALSE
  testthat::local_mocked_bindings(
    Sys.sleep = function(x) state$sleep_called <- TRUE,
    .package = "base"
  )

  tezr_env <- get("tezr_env", envir = asNamespace("tezr"))

  # Set last request to 5 seconds ago
  tezr_env$last_request <- Sys.time() - 5

  rate_limit(delay = 2)

  expect_false(state$sleep_called)
})

test_that("rate_limit does not sleep on first call", {
  local_clean_session()

  state <- new.env(parent = emptyenv())
  state$sleep_called <- FALSE
  testthat::local_mocked_bindings(
    Sys.sleep = function(x) state$sleep_called <- TRUE,
    .package = "base"
  )

  tezr_env <- get("tezr_env", envir = asNamespace("tezr"))

  # No previous timestamp
  tezr_env$last_request <- NULL

  rate_limit(delay = 2)

  expect_false(state$sleep_called)
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

  expect_identical(tezr_env$cookies, "JSESSIONID=abc123")
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

  state <- new.env(parent = emptyenv())
  state$refresh_called <- FALSE
  state$info_messages <- character()

  testthat::local_mocked_bindings(
    init_session = function(...) {
      state$refresh_called <- TRUE
      invisible(TRUE)
    },
    .package = "tezr"
  )
  testthat::local_mocked_bindings(
    cli_alert_info = function(text, ...) {
      state$info_messages <- c(state$info_messages, paste(text, collapse = " "))
      invisible(NULL)
    },
    .package = "cli"
  )

  refresh_session_if_needed()

  expect_true(state$refresh_called)
  expect_true(any(grepl(
    "request count",
    state$info_messages,
    ignore.case = TRUE
  )))
})

test_that("refresh_session_if_needed reports age-only and combined staleness", {
  local_clean_session()
  tezr_env <- get("tezr_env", envir = asNamespace("tezr"))
  state <- new.env(parent = emptyenv())
  state$info_messages <- character()

  testthat::local_mocked_bindings(
    init_session = function(...) invisible(TRUE),
    .package = "tezr"
  )
  testthat::local_mocked_bindings(
    cli_alert_info = function(text, ...) {
      state$info_messages <- c(state$info_messages, paste(text, collapse = " "))
      invisible(NULL)
    },
    .package = "cli"
  )

  tezr_env$request_count <- 0L
  tezr_env$session_start <- Sys.time() - (21 * 60)
  refresh_session_if_needed()

  tezr_env$request_count <- 50L
  tezr_env$session_start <- Sys.time() - (21 * 60)
  refresh_session_if_needed()

  expect_true(any(grepl("session age", state$info_messages, fixed = TRUE)))
  expect_true(any(grepl(
    "request count and session age",
    state$info_messages,
    fixed = TRUE
  )))
})
