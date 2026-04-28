# Tests for thesis detail functions (detail.R)

test_that("detail requires detail_id", {
  expect_error(detail(), "detail_id.*required")
})

test_that("detail rejects empty detail_id vectors", {
  expect_error(detail(character(0)), "non-empty")
})

test_that("detail rejects non-character input", {
  expect_error(detail(123), "must be a character vector")
  expect_error(detail(TRUE), "must be a character vector")
})

test_that("detail rejects NULL detail_id", {
  expect_error(detail(NULL), "detail_id.*required")
})

test_that("single fetch returns cached result when available", {
  fake_details <- list(
    thesis_no = "12345",
    title_original = "Test Thesis",
    author = "Test Author"
  )

  result <- testthat::with_mocked_bindings(
    detail("abc123"),
    fetch_single_thesis = function(tid) fake_details
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$thesis_no, "12345")
})

# --- Batch tests (parallel path) ---

make_fake_response <- function(detail_id, status_code = 200L) {
  fake_html <- paste0(
    "<html><body><div id='thesisDetail'>",
    "<table><tr><td>Tez No</td><td>",
    detail_id,
    "</td></tr>",
    "<tr><td>Yazar</td><td>Author ",
    detail_id,
    "</td></tr>",
    "</table></div></body></html>"
  )
  httr2::response(
    status_code = status_code,
    headers = list("Content-Type" = "text/html; charset=utf-8"),
    body = charToRaw(fake_html)
  )
}

test_that("batch fetch uses parallel and returns correct rows", {
  fake_parsed <- function(html) {
    # Extract thesis_no from the fake HTML
    raw_text <- as.character(html)
    list(
      thesis_no = "tid",
      title_original = "Test Thesis"
    )
  }

  fake_resps <- lapply(c("a", "b", "c"), make_fake_response)

  result <- testthat::with_mocked_bindings(
    detail(c("a", "b", "c"), progress = FALSE),
    init_cache = function() invisible(NULL),
    get_cached = function(...) NULL,
    has_session = function() TRUE,
    refresh_session_if_needed = function() invisible(NULL),
    build_detail_request = function(tid) httr2::request("https://example.com"),
    perform_parallel = function(reqs, ...) fake_resps,
    parse_detail_response = function(resp, tid) {
      list(thesis_no = tid, title_original = paste("Thesis", tid))
    },
    increment_request_count = function() invisible(NULL),
    set_cached = function(...) invisible(NULL),
    .package = "tezr"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(result$thesis_no, c("a", "b", "c"))
})

test_that("batch fetch uses cached results and skips network for cached IDs", {
  call_log <- character()

  result <- testthat::with_mocked_bindings(
    detail(c("aaa", "bbb", "ccc"), progress = FALSE),
    init_cache = function() invisible(NULL),
    get_cached = function(cache_env, key, ttl) {
      if (grepl("d_aaa_", key)) {
        return(list(thesis_no = "aaa", title_original = "Cached 1"))
      }
      if (grepl("d_ccc_", key)) {
        return(list(thesis_no = "ccc", title_original = "Cached 2"))
      }
      return(NULL)
    },
    has_session = function() TRUE,
    refresh_session_if_needed = function() invisible(NULL),
    build_detail_request = function(tid) {
      call_log <<- c(call_log, tid)
      httr2::request("https://example.com")
    },
    perform_parallel = function(reqs, ...) {
      lapply(seq_along(reqs), function(i) make_fake_response("x"))
    },
    parse_detail_response = function(resp, tid) {
      list(thesis_no = tid, title_original = paste("Fresh", tid))
    },
    increment_request_count = function() invisible(NULL),
    set_cached = function(...) invisible(NULL),
    .package = "tezr"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  # Order preserved

  expect_equal(result$thesis_no, c("aaa", "bbb", "ccc"))
  # Only uncached ID triggered request building
  expect_equal(call_log, "bbb")
})

test_that("batch fetch preserves original order with mixed cached/uncached", {
  result <- testthat::with_mocked_bindings(
    detail(c("u1", "c1", "u2", "c2"), progress = FALSE),
    init_cache = function() invisible(NULL),
    get_cached = function(cache_env, key, ttl) {
      if (grepl("c1", key)) {
        return(list(thesis_no = "c1", title_original = "Cached"))
      }
      if (grepl("c2", key)) {
        return(list(thesis_no = "c2", title_original = "Cached"))
      }
      return(NULL)
    },
    has_session = function() TRUE,
    refresh_session_if_needed = function() invisible(NULL),
    build_detail_request = function(tid) httr2::request("https://example.com"),
    perform_parallel = function(reqs, ...) {
      lapply(seq_along(reqs), function(i) make_fake_response("x"))
    },
    parse_detail_response = function(resp, tid) {
      list(thesis_no = tid, title_original = paste("Fresh", tid))
    },
    increment_request_count = function() invisible(NULL),
    set_cached = function(...) invisible(NULL),
    .package = "tezr"
  )

  expect_equal(result$thesis_no, c("u1", "c1", "u2", "c2"))
})

test_that("batch fetch handles parallel errors gracefully", {
  result <- testthat::with_mocked_bindings(
    detail(c("good", "bad"), progress = FALSE),
    init_cache = function() invisible(NULL),
    get_cached = function(...) NULL,
    has_session = function() TRUE,
    refresh_session_if_needed = function() invisible(NULL),
    build_detail_request = function(tid) httr2::request("https://example.com"),
    perform_parallel = function(reqs, ...) {
      list(
        make_fake_response("good"),
        make_fake_response("bad", status_code = 500L)
      )
    },
    parse_detail_response = function(resp, tid) {
      if (httr2::resp_status(resp) != 200L) {
        return(NULL)
      }
      list(thesis_no = tid, title_original = paste("Thesis", tid))
    },
    increment_request_count = function() invisible(NULL),
    set_cached = function(...) invisible(NULL),
    .package = "tezr"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$thesis_no, "good")
})

test_that("batch fetch with all failures returns empty tibble", {
  result <- testthat::with_mocked_bindings(
    detail(c("a", "b"), progress = FALSE),
    init_cache = function() invisible(NULL),
    get_cached = function(...) NULL,
    has_session = function() TRUE,
    refresh_session_if_needed = function() invisible(NULL),
    build_detail_request = function(tid) httr2::request("https://example.com"),
    perform_parallel = function(reqs, ...) {
      lapply(reqs, function(r) make_fake_response("x", status_code = 500L))
    },
    parse_detail_response = function(resp, tid) NULL,
    increment_request_count = function() invisible(NULL),
    set_cached = function(...) invisible(NULL),
    .package = "tezr"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("batch fetch skips NA detail_ids", {
  result <- testthat::with_mocked_bindings(
    detail(c("good", NA_character_), progress = FALSE),
    init_cache = function() invisible(NULL),
    get_cached = function(...) NULL,
    has_session = function() TRUE,
    refresh_session_if_needed = function() invisible(NULL),
    build_detail_request = function(tid) httr2::request("https://example.com"),
    perform_parallel = function(reqs, ...) {
      list(make_fake_response("good"))
    },
    parse_detail_response = function(resp, tid) {
      list(thesis_no = tid, title_original = "Test")
    },
    increment_request_count = function() invisible(NULL),
    set_cached = function(...) invisible(NULL),
    .package = "tezr"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$thesis_no, "good")
})

test_that("batch fetch with all cached skips network entirely", {
  network_called <- FALSE

  result <- testthat::with_mocked_bindings(
    detail(c("c1", "c2"), progress = FALSE),
    init_cache = function() invisible(NULL),
    get_cached = function(cache_env, key, ttl) {
      if (grepl("c1", key)) {
        return(list(thesis_no = "c1", title_original = "Cached 1"))
      }
      if (grepl("c2", key)) {
        return(list(thesis_no = "c2", title_original = "Cached 2"))
      }
      return(NULL)
    },
    has_session = function() TRUE,
    build_detail_request = function(tid) {
      network_called <<- TRUE
      httr2::request("https://example.com")
    },
    .package = "tezr"
  )

  expect_false(network_called)
  expect_equal(nrow(result), 2)
  expect_equal(result$thesis_no, c("c1", "c2"))
})

test_that("perform_parallel always disables httr2 progress output", {
  captured <- NULL

  testthat::with_mocked_bindings(
    perform_parallel(
      reqs = list(httr2::request("https://example.com"))
    ),
    req_perform_parallel = function(reqs, on_error, max_active, progress, ...) {
      captured <<- list(
        on_error = on_error,
        max_active = max_active,
        progress = progress
      )
      return(list())
    },
    .package = "httr2"
  )

  expect_equal(captured$on_error, "continue")
  expect_equal(captured$max_active, 5L)
  expect_false(captured$progress)
})

test_that("build_detail_request applies rate limiting by default", {
  rate_limit_flag <- NA

  testthat::with_mocked_bindings(
    build_detail_request("abc123"),
    create_session = function(ssl_verify = FALSE, apply_rate_limit = TRUE) {
      rate_limit_flag <<- apply_rate_limit
      return(httr2::request("https://example.com"))
    },
    .package = "tezr"
  )

  expect_true(rate_limit_flag)
})

test_that("build_detail_request can skip rate limiting via advanced option", {
  rate_limit_flag <- NA
  withr::local_options(list(tezr.detail_rate_limit = FALSE))

  testthat::with_mocked_bindings(
    build_detail_request("abc123"),
    create_session = function(ssl_verify = FALSE, apply_rate_limit = TRUE) {
      rate_limit_flag <<- apply_rate_limit
      return(httr2::request("https://example.com"))
    },
    .package = "tezr"
  )

  expect_false(rate_limit_flag)
})

test_that("fetch_uncached_parallel uses chunked batches when progress is enabled", {
  request_batch_sizes <- integer()
  seen_ids <- character()
  increment_calls <- 0L

  results <- testthat::with_mocked_bindings(
    fetch_uncached_parallel(
      uncached_ids = sprintf("id%03d", 1:61),
      results = list(),
      progress = TRUE
    ),
    has_session = function() TRUE,
    refresh_session_if_needed = function() invisible(NULL),
    build_detail_request = function(tid) tid,
    perform_parallel = function(reqs) {
      request_batch_sizes <<- c(request_batch_sizes, length(reqs))
      return(reqs)
    },
    parse_detail_response = function(resp, detail_id) {
      seen_ids <<- c(seen_ids, detail_id)
      return(list(thesis_no = detail_id))
    },
    increment_request_count = function() {
      increment_calls <<- increment_calls + 1L
      invisible(NULL)
    },
    .package = "tezr"
  )

  expect_equal(request_batch_sizes, c(10L, 10L, 10L, 10L, 10L, 10L, 1L))
  expect_equal(length(seen_ids), 61L)
  expect_equal(increment_calls, 61L)
  expect_equal(length(results), 61L)
})

test_that("fetch_uncached_parallel uses a single batch when progress is disabled", {
  request_batch_sizes <- integer()
  seen_ids <- character()

  results <- testthat::with_mocked_bindings(
    fetch_uncached_parallel(
      uncached_ids = sprintf("id%03d", 1:61),
      results = list(),
      progress = FALSE
    ),
    has_session = function() TRUE,
    refresh_session_if_needed = function() invisible(NULL),
    build_detail_request = function(tid) tid,
    perform_parallel = function(reqs) {
      request_batch_sizes <<- c(request_batch_sizes, length(reqs))
      return(reqs)
    },
    parse_detail_response = function(resp, detail_id) {
      seen_ids <<- c(seen_ids, detail_id)
      return(list(thesis_no = detail_id))
    },
    increment_request_count = function() invisible(NULL),
    .package = "tezr"
  )

  expect_equal(request_batch_sizes, 61L)
  expect_equal(length(seen_ids), 61L)
  expect_equal(length(results), 61L)
})
