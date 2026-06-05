# Tests for year-range pagination (paginate.R)

test_that("fetch_year_range_iterative handles non-200 responses", {
  form_builder <- function(...) list()

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) invisible(NULL),
    perform_search_request = function(...) {
      stop("Search request failed with status 500")
    },
    .package = "tezr"
  )
  local_silence_cli()

  result <- fetch_year_range_iterative(
    total_count = 100L,
    year_start = 2000,
    year_end = 2001,
    form_builder = form_builder,
    cache_key_params = list(),
    max_search_results = 10
  )

  expect_type(result, "list")
  # Each element should be a tibble (possibly empty)
  for (tbl in result) {
    expect_s3_class(tbl, "tbl_df")
  }
})

test_that("fetch_year_range_iterative does not sleep directly", {
  state <- new.env(parent = emptyenv())
  state$sleep_calls <- 0L

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) invisible(NULL),
    perform_search_request = function(...) {
      stop("Search request failed with status 500")
    },
    .package = "tezr"
  )
  local_silence_cli()
  testthat::local_mocked_bindings(
    Sys.sleep = function(...) state$sleep_calls <- state$sleep_calls + 1L,
    .package = "base"
  )

  fetch_year_range_iterative(
    total_count = 100L,
    year_start = 2000,
    year_end = 2001,
    form_builder = function(...) list(),
    cache_key_params = list(),
    max_search_results = 10
  )

  expect_identical(state$sleep_calls, 0L)
})

test_that("fetch_year_range_iterative defaults NULL cache parameters", {
  state <- new.env(parent = emptyenv())
  state$captured_key <- NULL

  testthat::local_mocked_bindings(
    get_cached = function(cache_env, key, ttl) {
      state$captured_key <- key
      NULL
    },
    set_cached = function(...) invisible(NULL),
    perform_search_request = function(...) {
      list(
        search_results = tibble::tibble(thesis_no = "1"),
        total_count = 1L
      )
    },
    .package = "tezr"
  )
  local_silence_cli()

  result <- fetch_year_range_iterative(
    total_count = 1L,
    year_start = 2020,
    year_end = 2020,
    form_builder = function(...) list(),
    cache_key_params = NULL,
    max_search_results = 10
  )

  expect_length(result, 1L)
  expect_type(state$captured_key, "character")
})

test_that("fetch_year_range_iterative warns without server-limit wording", {
  # Simulate a single-year range with returned rows below reported total.
  state <- new.env(parent = emptyenv())
  state$call_count <- 0L
  state$warnings_seen <- character()
  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) invisible(NULL),
    perform_search_request = function(...) {
      state$call_count <- state$call_count + 1L
      list(
        search_results = tibble::tibble(thesis_no = as.character(seq_len(500))),
        total_count = 600L
      )
    },
    .package = "tezr"
  )

  testthat::local_mocked_bindings(
    cli_alert_warning = function(text, ...) {
      state$warnings_seen <- c(state$warnings_seen, text)
    },
    cli_alert_info = function(...) NULL,
    .package = "cli"
  )

  fetch_year_range_iterative(
    total_count = 600L,
    year_start = 2020,
    year_end = 2020,
    form_builder = function(...) list(),
    cache_key_params = list(),
    max_search_results = 1000
  )

  expect_true(any(grepl("reported", state$warnings_seen, fixed = TRUE)))
  expect_false(any(grepl("server limit", state$warnings_seen, fixed = TRUE)))
})

test_that("fetch_year_range_iterative warns on unsplittable capped ranges", {
  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) invisible(NULL),
    perform_search_request = function(...) {
      list(
        search_results = tibble::tibble(
          thesis_no = as.character(seq_len(1999))
        ),
        total_count = 2500L
      )
    },
    .package = "tezr"
  )

  state <- new.env(parent = emptyenv())
  state$warnings_seen <- character()
  testthat::local_mocked_bindings(
    cli_alert_warning = function(text, ...) {
      state$warnings_seen <- c(state$warnings_seen, text)
    },
    cli_alert_info = function(...) NULL,
    .package = "cli"
  )

  fetch_year_range_iterative(
    total_count = 2500L,
    year_start = 2020,
    year_end = 2020,
    form_builder = function(...) list(),
    cache_key_params = list(),
    max_search_results = 5000
  )

  expect_true(any(grepl("server limit", state$warnings_seen, fixed = TRUE)))
  expect_true(any(grepl("single-year", state$warnings_seen, fixed = TRUE)))
  expect_true(any(grepl(
    "cannot split further",
    state$warnings_seen,
    fixed = TRUE
  )))
  expect_true(any(grepl("Add more filters", state$warnings_seen, fixed = TRUE)))
})

test_that("capped multi-year warnings omit single-year wording", {
  state <- new.env(parent = emptyenv())
  state$warnings_seen <- character()
  testthat::local_mocked_bindings(
    cli_alert_warning = function(text, ...) {
      state$warnings_seen <- c(state$warnings_seen, text)
      invisible(NULL)
    },
    .package = "cli"
  )

  warn_capped_year_range(
    r_start = 2020,
    r_end = 2021,
    range_total = 2500L,
    range_count = 1999L
  )

  expect_true(any(grepl("server limit", state$warnings_seen, fixed = TRUE)))
  expect_false(any(grepl("single-year", state$warnings_seen, fixed = TRUE)))
})

test_that("fetch_year_range_iterative uses cached range results", {
  state <- new.env(parent = emptyenv())
  state$network_calls <- 0L

  testthat::local_mocked_bindings(
    get_cached = function(cache_env, key, ttl) {
      list(
        search_results = tibble::tibble(thesis_no = c("1", "2")),
        total_count = 2L
      )
    },
    set_cached = function(...) invisible(NULL),
    perform_search_request = function(...) {
      state$network_calls <- state$network_calls + 1L
      list(search_results = tibble::tibble(thesis_no = "99"), total_count = 1L)
    },
    .package = "tezr"
  )
  local_silence_cli()

  result <- fetch_year_range_iterative(
    total_count = 100L,
    year_start = 2020,
    year_end = 2021,
    form_builder = function(...) list(),
    cache_key_params = list(),
    max_search_results = 1000
  )

  expect_identical(state$network_calls, 0L)
  expect_gt(length(result), 0L)
})

test_that("fetch_year_range_iterative ignore_cache bypasses range cache", {
  testthat::local_mocked_bindings(
    get_cached = function(...) stop("get_cached should not be called"),
    set_cached = function(...) stop("set_cached should not be called"),
    perform_search_request = function(...) {
      list(
        search_results = tibble::tibble(thesis_no = c("1", "2")),
        total_count = 2L
      )
    },
    .package = "tezr"
  )
  local_silence_cli()

  result <- fetch_year_range_iterative(
    total_count = 2L,
    year_start = 2020,
    year_end = 2020,
    form_builder = function(...) list(),
    cache_key_params = list(),
    max_search_results = 1000,
    ignore_cache = TRUE
  )

  expect_length(result, 1L)
  expect_identical(nrow(result[[1]]), 2L)
})

test_that("fetch_year_range_iterative re-splits when chunk hits server limit", {
  state <- new.env(parent = emptyenv())
  state$call_count <- 0L

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) invisible(NULL),
    perform_search_request = function(...) {
      state$call_count <- state$call_count + 1L
      if (state$call_count == 1) {
        # First call returns 1999 rows (hits limit), triggering re-split
        list(
          search_results = tibble::tibble(
            thesis_no = as.character(seq_len(1999))
          ),
          total_count = 3000L
        )
      } else {
        # Subsequent calls return small results
        list(
          search_results = tibble::tibble(
            thesis_no = as.character(state$call_count)
          ),
          total_count = 1L
        )
      }
    },
    .package = "tezr"
  )
  local_silence_cli()

  result <- fetch_year_range_iterative(
    total_count = 3000L,
    year_start = 2010,
    year_end = 2020,
    form_builder = function(...) list(),
    cache_key_params = list(),
    max_search_results = 50000
  )

  # Should have made more calls than the initial split due to re-splitting

  expect_gt(state$call_count, 2L)
})

test_that("fetch_year_range_iterative respects max_search_results", {
  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) invisible(NULL),
    perform_search_request = function(...) {
      list(
        search_results = tibble::tibble(thesis_no = as.character(seq_len(100))),
        total_count = 100L
      )
    },
    .package = "tezr"
  )
  local_silence_cli()

  result <- fetch_year_range_iterative(
    total_count = 4000L,
    year_start = 2010,
    year_end = 2020,
    form_builder = function(...) list(),
    cache_key_params = list(),
    max_search_results = 150
  )

  total_rows <- sum(vapply(result, nrow, integer(1)))
  # May overshoot by at most one chunk (100 rows)
  expect_lte(total_rows, 250L)
})

test_that("fetch_by_year_ranges falls back to current results", {
  current <- tibble::tibble(
    thesis_no = c("1", "2"),
    title_original = c("A", "B")
  )

  testthat::local_mocked_bindings(
    fetch_year_range_iterative = function(...) list(),
    .package = "tezr"
  )
  local_silence_cli()

  result <- fetch_by_year_ranges(
    total_count = 100L,
    current_search_results = current,
    max_search_results = 1000,
    year_start = 2020,
    year_end = 2021,
    form_builder = function(...) list(),
    cache_key_params = list()
  )

  expect_identical(result, current)
})

test_that("fetch_by_year_ranges combines and deduplicates results", {
  testthat::local_mocked_bindings(
    fetch_year_range_iterative = function(...) {
      list(
        tibble::tibble(thesis_no = c("1", "2"), title_original = c("A", "B")),
        tibble::tibble(thesis_no = c("2", "3"), title_original = c("B", "C"))
      )
    },
    .package = "tezr"
  )
  local_silence_cli()

  result <- fetch_by_year_ranges(
    total_count = 100L,
    current_search_results = tibble::tibble(thesis_no = character()),
    max_search_results = 1000,
    year_start = 2020,
    year_end = 2021,
    form_builder = function(...) list(),
    cache_key_params = list()
  )

  expect_identical(nrow(result), 3L)
  expect_identical(sort(result$thesis_no), c("1", "2", "3"))
})

test_that("calculate_split_points returns correct number of splits", {
  pts <- calculate_split_points(2000, 2020, 3)
  expect_length(pts, 2L)
  expect_true(all(pts >= 2000))
  expect_true(all(pts < 2020))
  expect_false(is.unsorted(pts))
})

test_that("calculate_split_points returns empty for n_chunks < 2", {
  pts <- calculate_split_points(2000, 2020, 1)
  expect_length(pts, 0L)
})

test_that("calculate_split_points handles density weighting", {
  # Range spanning sparse (pre-2000) and dense (post-2010) eras:
  # split points should skew earlier to balance load
  pts <- calculate_split_points(1980, 2024, 3)
  expect_length(pts, 2L)
  # First split should be well past 2000 since pre-2000 years are sparse
  expect_gte(pts[1], 2000)
})

test_that("build_ranges_from_split produces non-overlapping covering ranges", {
  ranges <- build_ranges_from_split(2000, 2020, 4500)
  # Should cover entire range
  expect_identical(ranges[[1]]$start, 2000)
  expect_identical(ranges[[length(ranges)]]$end, 2020)
  # No gaps

  for (i in seq_along(ranges)[-1]) {
    expect_identical(ranges[[i]]$start, ranges[[i - 1]]$end + 1L)
  }
})

test_that("build_ranges_from_split single-year range covers the year", {
  ranges <- build_ranges_from_split(2020, 2020, 100)
  expect_identical(ranges[[1]]$start, 2020)
  expect_identical(ranges[[length(ranges)]]$end, 2020)
  # No range should have start > end
  for (r in ranges) {
    expect_lte(r$start, r$end)
  }
})

test_that("build_ranges_from_split pre-splits dense ranges", {
  # 4542 would yield only 3 chunks at a hard 2000 target, which is prone to
  # capped probe requests in recent years. We expect safer pre-splitting.
  ranges <- build_ranges_from_split(2019, 2026, 4542)
  expect_gte(length(ranges), 4L)
})

test_that("target_chunk_size shrinks dense recent ranges", {
  sparse_chunk <- calculate_target_chunk_size(
    year_start = 1960,
    year_end = 1980,
    total_count = 4000,
    era_weights = default_era_weights()
  )
  dense_chunk <- calculate_target_chunk_size(
    year_start = 2019,
    year_end = 2026,
    total_count = 4000,
    era_weights = default_era_weights()
  )

  expect_lt(dense_chunk, sparse_chunk)
  expect_gte(dense_chunk, 900L)
  expect_lte(sparse_chunk, 1700L)
})

test_that("target_chunk_size handles empty ranges and very large totals", {
  expect_identical(
    calculate_target_chunk_size(
      year_start = 2021,
      year_end = 2020,
      total_count = 0
    ),
    1500L
  )

  regular_chunk <- calculate_target_chunk_size(
    year_start = 2019,
    year_end = 2026,
    total_count = 4000,
    era_weights = default_era_weights()
  )
  large_chunk <- calculate_target_chunk_size(
    year_start = 2019,
    year_end = 2026,
    total_count = 20000,
    era_weights = default_era_weights()
  )

  expect_lte(large_chunk, regular_chunk)
})

test_that("adaptive weights learn from observed density", {
  state <- initialize_density_state()
  state <- update_density_state(
    state,
    1990,
    1991,
    range_total = 20L,
    range_count = 20L
  )
  state <- update_density_state(
    state,
    2021,
    2022,
    range_total = 1000L,
    range_count = 1000L
  )

  weights <- effective_era_weights(state)

  expect_gt(weights[["post2010"]], weights[["pre2000"]])
})

test_that("adaptive density ignores capped ranges", {
  state <- initialize_density_state()
  state <- update_density_state(
    state,
    2021,
    2022,
    range_total = 3000L,
    range_count = 1999L
  )

  expect_identical(sum(state$obs_years), 0L)
})

test_that("adaptive density ignores zero observations", {
  state <- initialize_density_state()
  updated <- update_density_state(
    state,
    2021,
    2022,
    range_total = 0L,
    range_count = 0L
  )

  expect_identical(updated, state)
})

test_that("adaptive density ignores empty year spans", {
  state <- initialize_density_state()
  updated <- update_density_state(
    state,
    2021,
    2020,
    range_total = 1L,
    range_count = 1L
  )

  expect_identical(updated, state)
})

test_that("effective weights keep priors when observed rates are zero", {
  state <- initialize_density_state()
  state$obs_years[["post2010"]] <- 2L

  expect_identical(effective_era_weights(state), state$prior)
})

test_that("year_density_weight returns correct values for each era", {
  expect_identical(year_density_weight(1990), 1)
  expect_identical(year_density_weight(1999), 1)
  expect_identical(year_density_weight(2000), 3)
  expect_identical(year_density_weight(2005), 3)
  expect_identical(year_density_weight(2010), 3)
  expect_identical(year_density_weight(2011), 8)
  expect_identical(year_density_weight(2020), 8)
})
