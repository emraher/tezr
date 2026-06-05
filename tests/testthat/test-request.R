# Tests for request helpers (request.R)

# Mock the full perform_search_request pipeline for a successful 200 response.
# Pass tezr_overrides to override specific tezr bindings.
local_mock_search_pipeline <- function(
  tezr_overrides = list(),
  env = parent.frame()
) {
  tezr_defaults <- list(
    refresh_session_if_needed = function() NULL,
    increment_request_count = function() NULL,
    create_session = function(...) structure(list(), class = "httr2_request"),
    extract_total_count = function(...) 0L,
    parse_results_table = function(...) tibble::tibble()
  )
  tezr_bindings <- modifyList(tezr_defaults, tezr_overrides)
  do.call(
    testthat::local_mocked_bindings,
    c(tezr_bindings, list(.package = "tezr", .env = env))
  )

  testthat::local_mocked_bindings(
    req_url_path_append = function(req, ...) req,
    req_body_form = function(req, ...) req,
    req_perform = function(...) {
      structure(list(status = 200L), class = "httr2_response")
    },
    resp_status = function(resp) 200L,
    resp_body_html = function(...) {
      rvest::read_html("<html><body></body></html>")
    },
    .package = "httr2",
    .env = env
  )
}

test_that("resolve_lookup_id skips lookup for NULL input", {
  state <- new.env(parent = emptyenv())
  state$called <- FALSE
  lookup <- function(x) {
    state$called <- TRUE
    "1"
  }

  result <- resolve_lookup_id(NULL, lookup, "University")

  expect_false(state$called)
  expect_null(result)
})

test_that("resolve_lookup_id returns id and logs info", {
  state <- new.env(parent = emptyenv())
  state$called <- FALSE
  state$info <- NULL
  state$warn <- NULL
  lookup <- function(x) {
    state$called <- TRUE
    "5"
  }

  testthat::local_mocked_bindings(
    cli_alert_info = function(msg, ...) state$info <- msg,
    cli_warn = function(msg, ...) state$warn <- msg,
    .package = "cli"
  )

  result <- resolve_lookup_id("Foo University", lookup, "University")

  expect_true(state$called)
  expect_identical(result, "5")
  expect_type(state$info, "character")
  expect_null(state$warn)
})

test_that("resolve_lookup_id warns when id is not found", {
  state <- new.env(parent = emptyenv())
  state$warn <- NULL
  lookup <- function(x) NULL

  testthat::local_mocked_bindings(
    cli_warn = function(msg, ...) state$warn <- msg,
    .package = "cli"
  )

  result <- resolve_lookup_id("Missing University", lookup, "University")

  expect_null(result)
  expect_type(state$warn, "character")
})

test_that("normalize_basic_cache_entry ignores malformed payloads", {
  expect_null(normalize_basic_cache_entry(list(results = tibble::tibble())))
})

test_that("perform_search_request errors on non-200 search response", {
  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  fake_req_perform <- function(...) {
    state$calls <- state$calls + 1L
    structure(list(status = 500L), class = "httr2_response")
  }

  testthat::local_mocked_bindings(
    req_url_path_append = function(req, ...) req,
    req_body_form = function(req, ...) req,
    req_perform = fake_req_perform,
    resp_status = function(resp) resp$status,
    .package = "httr2"
  )
  testthat::local_mocked_bindings(
    cli_alert_warning = function(...) NULL,
    .package = "cli"
  )

  testthat::local_mocked_bindings(
    create_session = function(...) structure(list(), class = "httr2_request"),
    .package = "tezr"
  )

  expect_error(
    perform_search_request(list(dummy = "x")),
    "Search request failed with status 500"
  )
  expect_identical(state$calls, 1L)
})

test_that("perform_search_request calls refresh_session_if_needed", {
  state <- new.env(parent = emptyenv())
  state$refresh_called <- FALSE
  local_mock_search_pipeline(
    tezr_overrides = list(
      refresh_session_if_needed = function() state$refresh_called <- TRUE
    )
  )

  perform_search_request(list(dummy = "x"))
  expect_true(state$refresh_called)
})

test_that("perform_search_request calls increment_request_count", {
  state <- new.env(parent = emptyenv())
  state$increment_called <- FALSE
  local_mock_search_pipeline(
    tezr_overrides = list(
      increment_request_count = function() state$increment_called <- TRUE
    )
  )

  perform_search_request(list(dummy = "x"))
  expect_true(state$increment_called)
})

test_that("perform_search_request reuses one session and counts one request", {
  state <- new.env(parent = emptyenv())
  state$create_session_calls <- 0L
  state$increment_calls <- 0L
  request_id <- new.env(parent = emptyenv())
  request_id$value <- "session-request"

  testthat::local_mocked_bindings(
    refresh_session_if_needed = function() NULL,
    increment_request_count = function() {
      state$increment_calls <- state$increment_calls + 1L
      invisible(NULL)
    },
    create_session = function(...) {
      state$create_session_calls <- state$create_session_calls + 1L
      request_id
    },
    extract_total_count = function(...) 1L,
    parse_results_table = function(...) tibble::tibble(thesis_no = "1"),
    .package = "tezr"
  )

  testthat::local_mocked_bindings(
    req_url_path_append = function(req, ...) req,
    req_body_form = function(req, ...) req,
    req_perform = function(req, ...) {
      expect_identical(req, request_id)
      structure(list(status = 200L), class = "httr2_response")
    },
    resp_status = function(resp) resp$status,
    resp_body_html = function(...) {
      rvest::read_html("<html><body></body></html>")
    },
    .package = "httr2"
  )

  perform_search_request(list(dummy = "x"))

  expect_identical(state$create_session_calls, 1L)
  expect_identical(state$increment_calls, 1L)
})

test_that("perform_search_request errors on non-200 results page", {
  state <- new.env(parent = emptyenv())
  state$call_count <- 0L

  testthat::local_mocked_bindings(
    refresh_session_if_needed = function() NULL,
    increment_request_count = function() NULL,
    create_session = function(...) structure(list(), class = "httr2_request"),
    .package = "tezr"
  )
  testthat::local_mocked_bindings(
    req_url_path_append = function(req, ...) req,
    req_body_form = function(req, ...) req,
    req_perform = function(...) {
      state$call_count <- state$call_count + 1L
      if (state$call_count == 1) {
        # First call (search POST) succeeds
        structure(list(status = 200L), class = "httr2_response")
      } else {
        # Second call (results page) fails
        structure(list(status = 503L), class = "httr2_response")
      }
    },
    resp_status = function(resp) resp$status,
    .package = "httr2"
  )

  expect_error(
    perform_search_request(list(dummy = "x")),
    "Failed to retrieve results page"
  )
})

test_that("run_basic_search returns cached results without network", {
  cached <- tibble::tibble(thesis_no = c("1", "2"))

  testthat::local_mocked_bindings(
    get_cached = function(...) cached,
    .package = "tezr"
  )
  testthat::local_mocked_bindings(
    cli_alert_success = function(...) NULL,
    .package = "cli"
  )

  result <- run_basic_search(
    form_data = list(),
    cache_key = "test_key",
    cache_label = "test"
  )

  expect_identical(result$results, cached)
  expect_identical(result$total_count, 2L)
})

test_that("run_basic_search caches results after fetch", {
  state <- new.env(parent = emptyenv())
  state$cached_key <- NULL
  state$cached_value <- NULL

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(cache_env, key, value) {
      state$cached_key <- key
      state$cached_value <- value
    },
    init_session = function(...) NULL,
    perform_search_request = function(...) {
      list(
        html = NULL,
        total_count = 1L,
        search_results = tibble::tibble(thesis_no = "42")
      )
    },
    .package = "tezr"
  )
  testthat::local_mocked_bindings(
    cli_alert_info = function(...) NULL,
    cli_alert_success = function(...) NULL,
    .package = "cli"
  )

  result <- run_basic_search(
    form_data = list(),
    cache_key = "my_key",
    cache_label = "test"
  )

  expect_identical(state$cached_key, "my_key")
  expect_type(state$cached_value, "list")
  expect_identical(state$cached_value$total_count, 1L)
  expect_identical(nrow(state$cached_value$results), 1L)
  expect_identical(result$results$thesis_no, "42")
})

test_that("run_basic_search returns empty tibble for zero results", {
  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    init_session = function(...) NULL,
    perform_search_request = function(...) {
      list(html = NULL, total_count = 0L, search_results = tibble::tibble())
    },
    .package = "tezr"
  )
  testthat::local_mocked_bindings(
    cli_alert_info = function(...) NULL,
    cli_alert_success = function(...) NULL,
    .package = "cli"
  )

  result <- run_basic_search(
    form_data = list(),
    cache_key = "empty_key",
    cache_label = "test"
  )

  expect_identical(result$total_count, 0L)
  expect_s3_class(result$results, "tbl_df")
  expect_identical(nrow(result$results), 0L)
})
