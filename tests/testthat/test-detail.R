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
  expect_identical(result$thesis_no, "12345")
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

local_clean_detail_cache <- function(env = parent.frame()) {
  tezr <- get("tezr_env", envir = asNamespace("tezr"))
  old_enabled <- tezr$cache_enabled
  old_detail_ttl <- tezr$detail_ttl
  old_detail_cache <- tezr$detail_cache

  withr::defer(
    {
      tezr$cache_enabled <- old_enabled
      tezr$detail_ttl <- old_detail_ttl
      tezr$detail_cache <- old_detail_cache
    },
    envir = env
  )

  tezr$cache_enabled <- TRUE
  tezr$detail_ttl <- NULL
  tezr$detail_cache <- new.env(parent = emptyenv())
}

test_that("single fetch returns cached details directly", {
  local_silence_cli()
  local_clean_detail_cache()

  fake_details <- list(thesis_no = "cached-id", title_original = "Cached")
  result <- testthat::with_mocked_bindings(
    fetch_single_thesis("cached-id"),
    init_cache = function() invisible(NULL),
    get_cached = function(...) fake_details,
    .package = "tezr"
  )

  expect_identical(result, fake_details)
})

test_that("single fetch initializes session and parses fresh detail response", {
  local_silence_cli()
  state <- new.env(parent = emptyenv())
  state$initialized <- 0L
  state$parsed <- 0L
  state$incremented <- 0L

  testthat::local_mocked_bindings(
    req_perform = function(req, ...) make_fake_response("fresh-id"),
    .package = "httr2"
  )

  result <- testthat::with_mocked_bindings(
    fetch_single_thesis("fresh-id"),
    init_cache = function() invisible(NULL),
    get_cached = function(...) NULL,
    has_session = function() FALSE,
    init_session = function() {
      state$initialized <- state$initialized + 1L
      invisible(NULL)
    },
    refresh_session_if_needed = function() invisible(NULL),
    increment_request_count = function() {
      state$incremented <- state$incremented + 1L
      invisible(NULL)
    },
    create_session = function(...) httr2::request("https://example.com"),
    parse_and_cache_detail_response = function(resp, detail_id, on_http_error) {
      state$parsed <- state$parsed + 1L
      list(thesis_no = detail_id, status = on_http_error)
    },
    .package = "tezr"
  )

  expect_identical(result$thesis_no, "fresh-id")
  expect_identical(result$status, "abort")
  expect_identical(state$initialized, 1L)
  expect_identical(state$incremented, 1L)
  expect_identical(state$parsed, 1L)
})

test_that("uncached parallel fetch returns early for no ids", {
  local_silence_cli()
  state <- new.env(parent = emptyenv())
  state$initialized <- 0L

  result <- testthat::with_mocked_bindings(
    fetch_uncached_parallel(character(), list(), progress = FALSE),
    has_session = function() FALSE,
    init_session = function() {
      state$initialized <- state$initialized + 1L
      invisible(NULL)
    },
    refresh_session_if_needed = function() invisible(NULL),
    .package = "tezr"
  )

  expect_identical(result, list())
  expect_identical(state$initialized, 1L)
})

test_that("detail responses are parsed and cached on success", {
  local_clean_detail_cache()

  result <- testthat::with_mocked_bindings(
    parse_and_cache_detail_response(make_fake_response("cached"), "cached"),
    parse_detail_page = function(html) {
      list(thesis_no = "cached", title_original = "Parsed")
    },
    .package = "tezr"
  )

  tezr <- get("tezr_env", envir = asNamespace("tezr"))
  cached <- get_cached(
    tezr$detail_cache,
    make_detail_key("cached", ""),
    tezr$detail_ttl
  )

  expect_identical(result$thesis_no, "cached")
  expect_match(result$detail_url, "tezDetay[.]jsp[?]id=cached")
  expect_identical(cached$thesis_no, "cached")
})

test_that("detail responses include paired detail URLs when available", {
  local_clean_detail_cache()

  result <- testthat::with_mocked_bindings(
    parse_and_cache_detail_response(
      make_fake_response("cached"),
      compose_detail_id("kayit-abc", "tez-xyz")
    ),
    parse_detail_page = function(html) {
      list(thesis_no = "cached", title_original = "Parsed")
    },
    .package = "tezr"
  )

  expect_match(
    result$detail_url,
    "tezDetay[.]jsp[?]id=kayit-abc&no=tez-xyz"
  )
})

test_that("detail response parser handles HTTP and parse failures", {
  local_silence_cli()
  local_clean_detail_cache()

  expect_null(parse_detail_response(make_fake_response("bad", 500L), "bad"))
  expect_error(
    parse_and_cache_detail_response(
      make_fake_response("abort", 500L),
      "abort",
      on_http_error = "abort"
    ),
    "status 500"
  )

  result <- testthat::with_mocked_bindings(
    parse_detail_response(make_fake_response("broken"), "broken"),
    parse_and_cache_detail_response = function(...) {
      stop("broken markup")
    },
    .package = "tezr"
  )

  expect_null(result)
})

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
  expect_identical(nrow(result), 3L)
  expect_identical(result$thesis_no, c("a", "b", "c"))
})

test_that("batch fetch uses cached results and skips network for cached IDs", {
  state <- new.env(parent = emptyenv())
  state$call_log <- character()

  result <- testthat::with_mocked_bindings(
    detail(c("aaa", "bbb", "ccc"), progress = FALSE),
    init_cache = function() invisible(NULL),
    get_cached = function(cache_env, key, ttl) {
      if (grepl("d_aaa_", key, fixed = TRUE)) {
        return(list(thesis_no = "aaa", title_original = "Cached 1"))
      }
      if (grepl("d_ccc_", key, fixed = TRUE)) {
        return(list(thesis_no = "ccc", title_original = "Cached 2"))
      }
      return(NULL)
    },
    has_session = function() TRUE,
    refresh_session_if_needed = function() invisible(NULL),
    build_detail_request = function(tid) {
      state$call_log <- c(state$call_log, tid)
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
  expect_identical(nrow(result), 3L)
  # Order preserved

  expect_identical(result$thesis_no, c("aaa", "bbb", "ccc"))
  # Only uncached ID triggered request building
  expect_identical(state$call_log, "bbb")
})

test_that("batch fetch preserves original order with mixed cached/uncached", {
  result <- testthat::with_mocked_bindings(
    detail(c("u1", "c1", "u2", "c2"), progress = FALSE),
    init_cache = function() invisible(NULL),
    get_cached = function(cache_env, key, ttl) {
      if (grepl("c1", key, fixed = TRUE)) {
        return(list(thesis_no = "c1", title_original = "Cached"))
      }
      if (grepl("c2", key, fixed = TRUE)) {
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

  expect_identical(result$thesis_no, c("u1", "c1", "u2", "c2"))
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
  expect_identical(nrow(result), 1L)
  expect_identical(result$thesis_no, "good")
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
  expect_identical(nrow(result), 0L)
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
  expect_identical(nrow(result), 1L)
  expect_identical(result$thesis_no, "good")
})

test_that("batch fetch with all cached skips network entirely", {
  state <- new.env(parent = emptyenv())
  state$network_called <- FALSE

  result <- testthat::with_mocked_bindings(
    detail(c("c1", "c2"), progress = FALSE),
    init_cache = function() invisible(NULL),
    get_cached = function(cache_env, key, ttl) {
      if (grepl("c1", key, fixed = TRUE)) {
        return(list(thesis_no = "c1", title_original = "Cached 1"))
      }
      if (grepl("c2", key, fixed = TRUE)) {
        return(list(thesis_no = "c2", title_original = "Cached 2"))
      }
      return(NULL)
    },
    has_session = function() TRUE,
    build_detail_request = function(tid) {
      state$network_called <- TRUE
      httr2::request("https://example.com")
    },
    .package = "tezr"
  )

  expect_false(state$network_called)
  expect_identical(nrow(result), 2L)
  expect_identical(result$thesis_no, c("c1", "c2"))
})

test_that("perform_parallel always disables httr2 progress output", {
  state <- new.env(parent = emptyenv())
  state$captured <- NULL

  testthat::with_mocked_bindings(
    perform_parallel(
      reqs = list(httr2::request("https://example.com"))
    ),
    req_perform_parallel = function(reqs, on_error, max_active, progress, ...) {
      state$captured <- list(
        on_error = on_error,
        max_active = max_active,
        progress = progress
      )
      return(list())
    },
    .package = "httr2"
  )

  expect_identical(state$captured$on_error, "continue")
  expect_identical(state$captured$max_active, 5L)
  expect_false(state$captured$progress)
})

test_that("build_detail_request applies rate limiting by default", {
  state <- new.env(parent = emptyenv())
  state$rate_limit_flag <- NA

  testthat::with_mocked_bindings(
    build_detail_request("abc123"),
    create_session = function(ssl_verify = FALSE, apply_rate_limit = TRUE) {
      state$rate_limit_flag <- apply_rate_limit
      return(httr2::request("https://example.com"))
    },
    .package = "tezr"
  )

  expect_true(state$rate_limit_flag)
})

test_that("build_detail_request can skip rate limiting via advanced option", {
  state <- new.env(parent = emptyenv())
  state$rate_limit_flag <- NA
  withr::local_options(list(tezr.detail_rate_limit = FALSE))

  testthat::with_mocked_bindings(
    build_detail_request("abc123"),
    create_session = function(ssl_verify = FALSE, apply_rate_limit = TRUE) {
      state$rate_limit_flag <- apply_rate_limit
      return(httr2::request("https://example.com"))
    },
    .package = "tezr"
  )

  expect_false(state$rate_limit_flag)
})

test_that("build_detail_request sends legacy id only without encrypted no", {
  req <- testthat::with_mocked_bindings(
    build_detail_request("legacy-id"),
    create_session = function(ssl_verify = FALSE, apply_rate_limit = TRUE) {
      httr2::request(base_url)
    },
    .package = "tezr"
  )

  url <- req$url
  expect_match(url, "tezDetay[.]jsp")
  expect_match(url, "id=legacy-id")
  expect_no_match(url, "no=")
})

test_that("build_detail_request sends paired encoded identifiers", {
  req <- testthat::with_mocked_bindings(
    build_detail_request(compose_detail_id("kayit-abc", "tez-xyz")),
    create_session = function(ssl_verify = FALSE, apply_rate_limit = TRUE) {
      httr2::request(base_url)
    },
    .package = "tezr"
  )

  url <- req$url
  expect_match(url, "tezDetay[.]jsp")
  expect_match(url, "id=kayit-abc")
  expect_match(url, "no=tez-xyz")
})

test_that("fetch_uncached_parallel chunks progress batches", {
  state <- new.env(parent = emptyenv())
  state$request_batch_sizes <- integer()
  state$seen_ids <- character()
  state$increment_calls <- 0L

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
      state$request_batch_sizes <- c(state$request_batch_sizes, length(reqs))
      return(reqs)
    },
    parse_detail_response = function(resp, detail_id) {
      state$seen_ids <- c(state$seen_ids, detail_id)
      return(list(thesis_no = detail_id))
    },
    increment_request_count = function() {
      state$increment_calls <- state$increment_calls + 1L
      invisible(NULL)
    },
    .package = "tezr"
  )

  expect_identical(
    state$request_batch_sizes,
    c(10L, 10L, 10L, 10L, 10L, 10L, 1L)
  )
  expect_length(state$seen_ids, 61L)
  expect_identical(state$increment_calls, 61L)
  expect_length(results, 61L)
})

test_that("fetch_uncached_parallel uses one batch without progress", {
  state <- new.env(parent = emptyenv())
  state$request_batch_sizes <- integer()
  state$seen_ids <- character()

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
      state$request_batch_sizes <- c(state$request_batch_sizes, length(reqs))
      return(reqs)
    },
    parse_detail_response = function(resp, detail_id) {
      state$seen_ids <- c(state$seen_ids, detail_id)
      return(list(thesis_no = detail_id))
    },
    increment_request_count = function() invisible(NULL),
    .package = "tezr"
  )

  expect_identical(state$request_batch_sizes, 61L)
  expect_length(state$seen_ids, 61L)
  expect_length(results, 61L)
})
