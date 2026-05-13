# Tests for request helpers (request.R)

# Mock the full perform_search_request pipeline for a successful 200 response.
# Pass tezr_overrides to override specific tezr bindings (e.g. refresh/increment).
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

test_that("resolve_lookup_item returns canonical name and id", {
  info <- NULL
  lookup <- function(x) tibble::tibble(name = "ANKARA ÜNİVERSİTESİ", id = "3")

  testthat::local_mocked_bindings(
    cli_alert_info = function(msg, ...) info <<- msg,
    .package = "cli"
  )

  result <- resolve_lookup_item("Ankara Üniversitesi", lookup, "University")

  expect_equal(result$id, "3")
  expect_equal(result$name, "ANKARA ÜNİVERSİTESİ")
  expect_true(is.character(info))
})

test_that("perform_search_request errors on non-200 search response", {
  calls <- 0L
  fake_req_perform <- function(...) {
    calls <<- calls + 1L
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
  expect_equal(calls, 1L)
})

test_that("perform_search_request calls refresh_session_if_needed", {
  refresh_called <- FALSE
  local_mock_search_pipeline(
    tezr_overrides = list(
      refresh_session_if_needed = function() refresh_called <<- TRUE
    )
  )

  perform_search_request(list(dummy = "x"))
  expect_true(refresh_called)
})

test_that("perform_search_request calls increment_request_count", {
  increment_called <- FALSE
  local_mock_search_pipeline(
    tezr_overrides = list(
      increment_request_count = function() increment_called <<- TRUE
    )
  )

  perform_search_request(list(dummy = "x"))
  expect_true(increment_called)
})

test_that("perform_search_request reuses one session object and counts one logical request", {
  create_session_calls <- 0L
  increment_calls <- 0L
  request_id <- new.env(parent = emptyenv())
  request_id$value <- "session-request"

  testthat::local_mocked_bindings(
    refresh_session_if_needed = function() NULL,
    increment_request_count = function() {
      increment_calls <<- increment_calls + 1L
      invisible(NULL)
    },
    create_session = function(...) {
      create_session_calls <<- create_session_calls + 1L
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

  expect_equal(create_session_calls, 1L)
  expect_equal(increment_calls, 1L)
})

test_that("perform_search_request resets the session when search operation changes", {
  init_calls <- 0L
  old_has_last_search_mode <- exists(
    "last_search_mode",
    envir = tezr_env,
    inherits = FALSE
  )
  old_last_search_mode <- tezr_env$last_search_mode
  withr::defer({
    if (old_has_last_search_mode) {
      tezr_env$last_search_mode <- old_last_search_mode
    } else if (exists("last_search_mode", envir = tezr_env, inherits = FALSE)) {
      rm(list = "last_search_mode", envir = tezr_env)
    }
  })

  tezr_env$last_search_mode <- "4"

  local_mock_search_pipeline(
    tezr_overrides = list(
      init_session = function(...) {
        init_calls <<- init_calls + 1L
        rm(list = "last_search_mode", envir = tezr_env)
        invisible(TRUE)
      }
    )
  )

  perform_search_request(list(islem = 2L))

  expect_equal(init_calls, 1L)
  expect_equal(tezr_env$last_search_mode, "2")
})

test_that("perform_search_request errors on non-200 results page", {
  call_count <- 0L

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
      call_count <<- call_count + 1L
      if (call_count == 1) {
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

  expect_equal(result$results, cached)
  expect_equal(result$total_count, 2L)
})

test_that("run_basic_search caches results after fetch", {
  cached_key <- NULL
  cached_value <- NULL

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(cache_env, key, value) {
      cached_key <<- key
      cached_value <<- value
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

  expect_equal(cached_key, "my_key")
  expect_type(cached_value, "list")
  expect_equal(cached_value$total_count, 1L)
  expect_equal(nrow(cached_value$results), 1)
  expect_equal(result$results$thesis_no, "42")
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

  expect_equal(result$total_count, 0L)
  expect_s3_class(result$results, "tbl_df")
  expect_equal(nrow(result$results), 0)
})
