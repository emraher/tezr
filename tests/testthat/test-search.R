# Tests for search functions (search.R)

test_that("search_basic validates query input", {
  expect_error(search_basic(), "keyword")
  expect_error(search_basic(""), "non-empty")
  expect_error(search_basic(123), "single non-empty character string")
  expect_error(search_basic(c("a", "b")), "single non-empty character string")
})

test_that("search_basic validates enum parameters", {
  expect_error(search_basic("test", search_field = "invalid"), "must be one of")
  expect_error(search_basic("test", thesis_type = "invalid"), "must be one of")
  expect_error(search_basic("test", access_type = "invalid"), "must be one of")
})

test_that("search_basic returns cached results without network", {
  cached <- tibble::tibble(thesis_no = "1")

  testthat::local_mocked_bindings(
    run_basic_search = function(...) list(results = cached, total_count = 1L)
  )

  result <- search_basic("test")
  expect_named(result, names(cached))
  expect_identical(result$thesis_no, cached$thesis_no)
  expect_identical(attr(result, "total_count", exact = TRUE), 1L)
  expect_false(attr(result, "paginated", exact = TRUE))
  expect_true(attr(result, "complete", exact = TRUE))
})

test_that("search_basic uses shared basic search helper", {
  state <- new.env(parent = emptyenv())
  state$called <- FALSE
  fake <- function(...) {
    state$called <- TRUE
    list(results = tibble::tibble(), total_count = 0L)
  }

  testthat::local_mocked_bindings(
    run_basic_search = fake
  )

  search_basic("test")
  expect_true(state$called)
})

test_that("search_basic forwards ignore_cache to run_basic_search", {
  state <- new.env(parent = emptyenv())
  state$captured_ignore_cache <- NULL

  testthat::local_mocked_bindings(
    run_basic_search = function(
      form_data,
      cache_key,
      cache_label,
      ignore_cache = FALSE
    ) {
      state$captured_ignore_cache <- ignore_cache
      list(results = empty_results_tibble(), total_count = 0L)
    }
  )

  search_basic("test", ignore_cache = TRUE)
  expect_true(isTRUE(state$captured_ignore_cache))
})

test_that("search_advanced validates keyword input", {
  expect_error(search_advanced(), "keyword")
  expect_error(search_advanced(keyword = ""), "non-empty")
  expect_error(
    search_advanced(keyword = 123),
    "single non-empty character string"
  )
  expect_error(
    search_advanced(keyword = c("a", "b")),
    "single non-empty character string"
  )
})

test_that("search_advanced validates enum parameters", {
  expect_error(
    search_advanced("test", group = "invalid"),
    "must be one of"
  )
  expect_error(
    search_advanced("test", status = "invalid"),
    "must be one of"
  )
  expect_error(
    search_advanced("test", search_field = "invalid"),
    "must be one of"
  )
})

test_that("search_advanced validates year range", {
  expect_error(
    search_advanced("test", year_start = 2020, year_end = 2015),
    "year_start.*must be less than or equal to.*year_end"
  )
  expect_error(
    search_advanced("test", year_start = 1800),
    "between 1959"
  )
})

test_that("search_advanced validates year_end", {
  expect_error(
    search_advanced("test", year_end = 1800),
    "between 1959"
  )
})

test_that("search_advanced returns cached results without network", {
  cached <- tibble::tibble(thesis_no = "1")

  testthat::local_mocked_bindings(
    get_cached = function(...) cached
  )

  result <- search_advanced("test")
  expect_identical(result, cached)
})

test_that("search_advanced ignore_cache bypasses read/write cache", {
  testthat::local_mocked_bindings(
    get_cached = function(...) stop("get_cached should not be called"),
    set_cached = function(...) stop("set_cached should not be called"),
    has_session = function(...) TRUE,
    init_session = function(...) NULL,
    perform_search_request = function(...) {
      list(
        total_count = 1L,
        search_results = tibble::tibble(thesis_no = "1")
      )
    }
  )

  result <- search_advanced("test", ignore_cache = TRUE)
  expect_identical(nrow(result), 1L)
})

test_that("search_advanced paginates when year range provided", {
  state <- new.env(parent = emptyenv())
  state$called <- FALSE

  testthat::local_mocked_bindings(
    perform_search_request = function(...) {
      list(
        total_count = 3000L,
        search_results = tibble::tibble(thesis_no = "1")
      )
    },
    fetch_by_year_ranges = function(...) {
      state$called <- TRUE
      tibble::tibble(thesis_no = c("1", "2", "3"))
    },
    init_session = function(...) NULL
  )

  search_advanced(
    "test",
    year_start = 2000,
    year_end = 2001,
    max_search_results = 3001
  )

  expect_true(state$called)
})

test_that("search_advanced forwards institution filters", {
  captured <- new.env(parent = emptyenv())
  captured$form <- NULL

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) NULL,
    init_session = function(...) NULL,
    lookup_university_id = function(name) {
      expect_identical(name, "Test Uni")
      "123"
    },
    lookup_institute_id = function(name) {
      expect_identical(name, "Test Inst")
      "456"
    },
    perform_search_request = function(form_data) {
      captured$form <- form_data
      list(
        total_count = 0L,
        search_results = tibble::tibble(thesis_no = character())
      )
    }
  )

  result <- search_advanced(
    keyword = "test",
    university = "Test Uni",
    institute = "Test Inst"
  )

  expect_identical(nrow(result), 0L)
  expect_identical(captured$form$uniad, "Test Uni")
  expect_identical(captured$form$Universite, 123L)
  expect_identical(captured$form$ensad, "Test Inst")
  expect_identical(captured$form$Enstitu, 456L)
})

test_that("search_advanced defaults to title field", {
  captured <- new.env(parent = emptyenv())
  captured$form <- NULL

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) NULL,
    init_session = function(...) NULL,
    perform_search_request = function(form_data) {
      captured$form <- form_data
      list(
        total_count = 0L,
        search_results = tibble::tibble(thesis_no = character())
      )
    }
  )

  result <- search_advanced(
    keyword = "sulama",
    year_start = 2015,
    year_end = 2024
  )

  expect_identical(nrow(result), 0L)
  expect_identical(captured$form$nevi, search_field_codes$title)
  expect_identical(captured$form$tip, match_type_codes$exact)
  expect_identical(captured$form$Tur, thesis_type_codes$all)
  expect_identical(captured$form$Dil, 0L)
  expect_identical(captured$form$izin, access_type_codes$all)
  expect_identical(captured$form$Durum, status_codes$approved)
})

test_that("search_advanced with Inf reports single-year overflow", {
  state <- new.env(parent = emptyenv())
  state$warnings_seen <- character()
  paginated_results <- tibble::tibble(thesis_no = as.character(seq_len(28584)))
  attr(paginated_results, "single_year_overflow_years") <- 2019L

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) NULL,
    has_session = function(...) TRUE,
    init_session = function(...) NULL,
    perform_search_request = function(...) {
      list(
        total_count = 28882L,
        search_results = tibble::tibble(thesis_no = as.character(seq_len(2000)))
      )
    },
    fetch_by_year_ranges = function(...) paginated_results
  )

  testthat::local_mocked_bindings(
    cli_alert_warning = function(text, ...) {
      state$warnings_seen <- c(
        state$warnings_seen,
        paste(text, collapse = " ")
      )
      invisible(NULL)
    },
    .package = "cli"
  )

  result <- search_advanced(
    keyword = "avrupa",
    search_field = "all",
    max_search_results = Inf
  )

  expect_identical(nrow(result), 28584L)
  expect_true(any(grepl(
    "single-year",
    state$warnings_seen,
    ignore.case = TRUE
  )))
  expect_true(any(grepl("2019", state$warnings_seen, fixed = TRUE)))
  expect_false(any(grepl(
    "max_search_results = Inf",
    state$warnings_seen,
    fixed = TRUE
  )))
})

test_that("search_advanced with Inf bypasses cached capped results", {
  cache_store <- new.env(parent = emptyenv())
  state <- new.env(parent = emptyenv())
  state$paginated <- FALSE
  paginated_results <- tibble::tibble(thesis_no = as.character(seq_len(3000)))

  testthat::local_mocked_bindings(
    init_cache = function(...) NULL,
    get_cached = function(cache_env, key, ttl = NULL) {
      if (exists(key, envir = cache_store, inherits = FALSE)) {
        get(key, envir = cache_store, inherits = FALSE)
      } else {
        NULL
      }
    },
    set_cached = function(cache_env, key, value) {
      assign(key, value, envir = cache_store)
      invisible(NULL)
    },
    has_session = function(...) TRUE,
    init_session = function(...) NULL,
    perform_search_request = function(...) {
      list(
        total_count = 5000L,
        search_results = tibble::tibble(
          thesis_no = as.character(seq_len(2000))
        )
      )
    },
    fetch_by_year_ranges = function(...) {
      state$paginated <- TRUE
      paginated_results
    }
  )

  first <- search_advanced(keyword = "education")
  second <- search_advanced(keyword = "education", max_search_results = Inf)

  expect_identical(nrow(first), 2000L)
  expect_true(state$paginated)
  expect_identical(nrow(second), 3000L)
})

test_that("build_search_cache_key matches previous behavior", {
  key_old <- make_search_key(type = "basic", query = "x", field = "all")
  key_new <- build_search_cache_key(
    type = "basic",
    params = list(query = "x", field = "all")
  )

  expect_identical(key_new, key_old)
})

test_that("search pipeline helpers handle cache and pagination decisions", {
  cached <- tibble::tibble(thesis_no = c("1", "2", "3"))

  expect_identical(
    return_cached_search_result(cached, max_search_results = 2)$thesis_no,
    c("1", "2")
  )

  auto_range <- resolve_auto_year_range(
    total_count = 3000L,
    max_search_results = 3000L,
    year_start = NULL,
    year_end = NULL
  )
  expect_true(auto_range$auto_year_range)
  expect_identical(auto_range$year_start, 1959L)

  fixed_range <- resolve_auto_year_range(
    total_count = 3000L,
    max_search_results = 3000L,
    year_start = 2020L,
    year_end = 2021L
  )
  expect_false(fixed_range$auto_year_range)
  expect_identical(fixed_range$year_start, 2020L)

  expect_true(needs_year_range_pagination(
    total_count = 10L,
    search_results = tibble::tibble(thesis_no = "1"),
    max_search_results = 10L,
    year_start = 2020L,
    year_end = 2021L
  ))
  expect_false(needs_year_range_pagination(
    total_count = 1L,
    search_results = tibble::tibble(thesis_no = "1"),
    max_search_results = 10L,
    year_start = 2020L,
    year_end = 2021L
  ))

  expect_identical(resolve_range_cache_key_params(list(a = 1)), list(a = 1))
  expect_identical(
    resolve_range_cache_key_params(function() list(a = 2)),
    list(a = 2)
  )
})

test_that("search helper formatting and cache parameter closures are stable", {
  expect_identical(format_overflow_years(integer(0)), "")
  expect_identical(
    format_overflow_years(2020:2028, max_years = 3L),
    "2020, 2021, 2022 and 6 more"
  )

  advanced_args <- list(
    keyword = "climate",
    search_field = "title",
    thesis_type = "phd",
    access_type = "open",
    language = "tr",
    group = "science",
    university = "Test University",
    institute = "Test Institute",
    status = "approved",
    match_type = "exact"
  )
  advanced_ids <- new.env(parent = emptyenv())
  advanced_ids$university_id <- 1L
  advanced_ids$institute_id <- 2L

  advanced_params <- advanced_cache_key_params(advanced_args, advanced_ids)()
  expect_identical(advanced_params$university_id, 1L)
  expect_identical(advanced_params$institute_id, 2L)

  detailed_args <- list(
    thesis_no = "123",
    title = "Title",
    author = "Author",
    supervisor = "Supervisor",
    abstract = "Abstract",
    keyword = "Keyword",
    thesis_type = "masters",
    language = "en",
    access_type = "restricted",
    group = "social",
    status = "approved"
  )
  detailed_ids <- new.env(parent = emptyenv())
  detailed_ids$university_id <- 1L
  detailed_ids$institute_id <- 2L
  detailed_ids$division_id <- 3L
  detailed_ids$subject_id <- 4L
  detailed_ids$discipline_id <- 5L

  detailed_params <- detailed_cache_key_params(detailed_args, detailed_ids)()
  expect_identical(detailed_params$subject_id, 4L)
  expect_identical(detailed_params$discipline_id, 5L)
})

test_that("finish_search_pipeline_results trims rows after caching", {
  rows <- tibble::tibble(thesis_no = c("1", "2", "3"))

  result <- finish_search_pipeline_results(
    search_results = rows,
    total_count = 3L,
    fetched_count = 3L,
    paginated = FALSE,
    auto_year_range = FALSE,
    single_year_overflow_years = integer(),
    cache_key = "trim_test",
    max_search_results = 2L,
    requested_all_results = FALSE,
    ignore_cache = TRUE
  )

  expect_identical(result$thesis_no, c("1", "2"))
})

test_that("search_basic does not hit network when run_basic_search mocked", {
  testthat::local_mocked_bindings(
    run_basic_search = function(...) {
      list(results = empty_results_tibble(), total_count = 0L)
    }
  )

  result <- search_basic("test")
  expect_identical(nrow(result), 0L)
})

test_that("search_basic warns at 2000 with default max_search_results", {
  fake_results <- tibble::tibble(thesis_no = as.character(seq_len(2000)))

  testthat::local_mocked_bindings(
    run_basic_search = function(...) {
      list(results = fake_results, total_count = 5000L)
    }
  )

  expect_message(
    search_basic("test"),
    "max_search_results = Inf"
  )
})

test_that("search_basic delegates when max_search_results exceeds 2000", {
  state <- new.env(parent = emptyenv())
  state$delegated <- FALSE
  state$advanced_args <- list()
  fake_results <- tibble::tibble(thesis_no = as.character(seq_len(2000)))
  advanced_results <- tibble::tibble(thesis_no = as.character(seq_len(3000)))

  testthat::local_mocked_bindings(
    run_basic_search = function(...) {
      list(results = fake_results, total_count = 5000L)
    },
    search_advanced = function(
      keyword,
      search_field,
      thesis_type,
      access_type,
      match_type,
      max_search_results,
      ...
    ) {
      state$delegated <- TRUE
      state$advanced_args <- list(
        keyword = keyword,
        search_field = search_field,
        thesis_type = thesis_type,
        access_type = access_type,
        match_type = match_type,
        max_search_results = max_search_results
      )
      return(advanced_results)
    }
  )

  result <- search_basic(
    "test query",
    search_field = "title",
    thesis_type = "phd",
    access_type = "open",
    max_search_results = Inf
  )

  expect_true(state$delegated)
  expect_identical(state$advanced_args$keyword, "test query")
  expect_identical(state$advanced_args$search_field, "title")
  expect_identical(state$advanced_args$thesis_type, "phd")
  expect_identical(state$advanced_args$match_type, "contains")
  expect_identical(state$advanced_args$access_type, "open")
  expect_identical(nrow(result), 3000L)
})

test_that("search_basic delegation reuses one initialized session", {
  state <- new.env(parent = emptyenv())
  state$init_calls <- 0L
  state$request_calls <- 0L

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) NULL,
    init_session = function(...) {
      state$init_calls <- state$init_calls + 1L
      tezr_env$cookies <- "JSESSIONID=test"
      tezr_env$session_start <- Sys.time()
      tezr_env$request_count <- 0L
      invisible(TRUE)
    },
    perform_search_request = function(...) {
      state$request_calls <- state$request_calls + 1L
      if (state$request_calls == 1L) {
        return(list(
          total_count = 5965L,
          search_results = tibble::tibble(
            thesis_no = as.character(seq_len(2000))
          )
        ))
      }

      return(list(
        total_count = 10L,
        search_results = tibble::tibble(
          thesis_no = as.character(seq_len(10))
        )
      ))
    }
  )

  result <- search_basic(keyword = "climate change", max_search_results = Inf)

  expect_identical(state$init_calls, 1L)
  expect_identical(nrow(result), 10L)
})

test_that("search_basic does not delegate when results fit in 2000", {
  fake_results <- tibble::tibble(thesis_no = as.character(seq_len(500)))

  testthat::local_mocked_bindings(
    run_basic_search = function(...) {
      list(results = fake_results, total_count = 500L)
    }
  )

  result <- search_basic("test", max_search_results = Inf)
  expect_identical(nrow(result), 500L)
})

test_that("search_basic with Inf delegates after truncated result is cached", {
  cache_store <- new.env(parent = emptyenv())
  state <- new.env(parent = emptyenv())
  state$delegated <- FALSE
  advanced_results <- tibble::tibble(thesis_no = as.character(seq_len(3000)))

  testthat::local_mocked_bindings(
    init_cache = function(...) NULL,
    get_cached = function(cache_env, key, ttl = NULL) {
      if (exists(key, envir = cache_store, inherits = FALSE)) {
        get(key, envir = cache_store, inherits = FALSE)
      } else {
        NULL
      }
    },
    set_cached = function(cache_env, key, value) {
      assign(key, value, envir = cache_store)
      invisible(NULL)
    },
    has_session = function(...) TRUE,
    init_session = function(...) NULL,
    perform_search_request = function(...) {
      list(
        total_count = 5968L,
        search_results = tibble::tibble(
          thesis_no = as.character(seq_len(2000))
        )
      )
    },
    search_advanced = function(...) {
      state$delegated <- TRUE
      advanced_results
    }
  )

  first <- search_basic(keyword = "climate change")
  second <- search_basic(keyword = "climate change", max_search_results = Inf)

  expect_identical(nrow(first), 2000L)
  expect_true(state$delegated)
  expect_identical(nrow(second), 3000L)
})

test_that("search_detailed requires at least one criterion", {
  expect_error(search_detailed(), "At least one search")
})

test_that("search_detailed validates enum parameters", {
  expect_error(
    search_detailed(author = "test", thesis_type = "invalid"),
    "Invalid.*thesis_type"
  )
  expect_error(
    search_detailed(author = "test", access_type = "invalid"),
    "Invalid.*access_type"
  )
})

test_that("search_detailed validates year range", {
  expect_error(
    search_detailed(author = "test", year_start = 1800),
    "between 1959"
  )
})

test_that("search_detailed errors when year_start is after year_end", {
  expect_error(
    search_detailed(author = "test", year_start = 2021, year_end = 2020),
    "year_start.*year_end"
  )
})

test_that("search_detailed uses expansion for multi-value inputs", {
  called <- new.env(parent = emptyenv())
  expected <- tibble::tibble(thesis_no = "1")

  fake_expand <- function(...) {
    called$args <- list(...)
    expected
  }

  testthat::local_mocked_bindings(
    expand_and_search_detailed = fake_expand
  )

  result <- search_detailed(author = "test", thesis_type = c("phd", "masters"))

  expect_identical(result, expected)
  expect_identical(called$args$thesis_type, c("phd", "masters"))
})

test_that("search_detailed expansion skips combined cap when max is omitted", {
  state <- new.env(parent = emptyenv())
  state$captured_limit_flag <- NULL
  expected <- tibble::tibble(thesis_no = as.character(seq_len(4036)))

  testthat::local_mocked_bindings(
    expand_and_search_detailed = function(..., limit_combined_results = TRUE) {
      state$captured_limit_flag <- limit_combined_results
      expected
    }
  )

  result <- search_detailed(subject = c("Ekonomi", "Iktisat"))

  expect_identical(nrow(result), 4036L)
  expect_false(isTRUE(state$captured_limit_flag))
})

test_that("search_detailed trims cached results to max_search_results", {
  cached <- tibble::tibble(thesis_no = c("1", "2", "3"))

  testthat::local_mocked_bindings(
    get_cached = function(...) cached
  )

  result <- search_detailed(author = "test", max_search_results = 2)

  expect_identical(nrow(result), 2L)
  expect_identical(result$thesis_no, c("1", "2"))
})

test_that("search_detailed with Inf bypasses cached capped results", {
  cache_store <- new.env(parent = emptyenv())
  state <- new.env(parent = emptyenv())
  state$paginated <- FALSE
  paginated_results <- tibble::tibble(thesis_no = as.character(seq_len(3500)))

  testthat::local_mocked_bindings(
    init_cache = function(...) NULL,
    get_cached = function(cache_env, key, ttl = NULL) {
      if (exists(key, envir = cache_store, inherits = FALSE)) {
        get(key, envir = cache_store, inherits = FALSE)
      } else {
        NULL
      }
    },
    set_cached = function(cache_env, key, value) {
      assign(key, value, envir = cache_store)
      invisible(NULL)
    },
    has_session = function(...) TRUE,
    init_session = function(...) NULL,
    lookup_subject_id = function(...) "35",
    perform_search_request = function(...) {
      list(
        total_count = 5000L,
        search_results = tibble::tibble(
          thesis_no = as.character(seq_len(2000))
        )
      )
    },
    fetch_by_year_ranges = function(...) {
      state$paginated <- TRUE
      paginated_results
    }
  )

  first <- search_detailed(subject = "Economics")
  second <- search_detailed(subject = "Economics", max_search_results = Inf)

  expect_identical(nrow(first), 2000L)
  expect_true(state$paginated)
  expect_identical(nrow(second), 3500L)
})

test_that("search_detailed ignore_cache bypasses read/write cache", {
  testthat::local_mocked_bindings(
    get_cached = function(...) stop("get_cached should not be called"),
    set_cached = function(...) stop("set_cached should not be called"),
    has_session = function(...) TRUE,
    init_session = function(...) NULL,
    perform_search_request = function(...) {
      list(
        total_count = 1L,
        search_results = tibble::tibble(thesis_no = "1")
      )
    }
  )

  result <- search_detailed(author = "test", ignore_cache = TRUE)
  expect_identical(nrow(result), 1L)
})

test_that("search_detailed validates status parameter", {
  expect_error(
    search_detailed(author = "test", status = "invalid"),
    "Invalid.*status"
  )
})

test_that("search_detailed accepts form IDs and skips lookups", {
  captured <- new.env(parent = emptyenv())
  captured$form <- NULL

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) NULL,
    has_session = function(...) TRUE,
    init_session = function(...) NULL,
    lookup_university_id = function(...) {
      stop("university lookup should not be called")
    },
    lookup_institute_id = function(...) {
      stop("institute lookup should not be called")
    },
    lookup_division_id = function(...) {
      stop("division lookup should not be called")
    },
    lookup_discipline_id = function(...) {
      stop("discipline lookup should not be called")
    },
    perform_search_request = function(form_data) {
      captured$form <- form_data
      list(
        total_count = 0L,
        search_results = tibble::tibble(thesis_no = character())
      )
    }
  )

  result <- search_detailed(
    university_id = 6L,
    institute_id = 12L,
    division_id = 128L,
    discipline_id = 512L,
    group = "science"
  )

  expect_identical(nrow(result), 0L)
  expect_identical(captured$form$Universite, 6L)
  expect_identical(captured$form$Enstitu, 12L)
  expect_identical(captured$form$ABD, 128L)
  expect_identical(captured$form$BilimDali, 512L)
  expect_identical(captured$form$EnstituGrubu, group_codes$science)
})

test_that("search_detailed validates optional institution ID arguments", {
  expect_error(
    search_detailed(university_id = 0),
    "university_id.*single positive integer"
  )
  expect_error(
    search_detailed(institute_id = c(1, 2)),
    "institute_id.*single positive integer"
  )
  expect_error(
    search_detailed(division_id = "bad"),
    "division_id.*single positive integer"
  )
  expect_error(
    search_detailed(discipline_id = -1),
    "discipline_id.*single positive integer"
  )
})

test_that("expand_and_search_detailed deduplicates combined results", {
  state <- new.env(parent = emptyenv())
  state$call_count <- 0L
  fake_search <- function(...) {
    state$call_count <- state$call_count + 1L
    # Return overlapping results
    tibble::tibble(
      thesis_no = c("1", "2"),
      title_original = c("A", "B")
    )
  }

  testthat::local_mocked_bindings(
    search_detailed = fake_search
  )

  result <- expand_and_search_detailed(
    author = c("Author A", "Author B"),
    thesis_type = "all",
    access_type = "all"
  )

  # Should deduplicate thesis_no "1" and "2" that appear in both searches
  expect_identical(nrow(result), 2L)
  expect_identical(result$thesis_no, c("1", "2"))
})

test_that("expand_and_search_detailed returns empty tibble on failures", {
  testthat::local_mocked_bindings(
    search_detailed = function(...) stop("Network error")
  )
  testthat::local_mocked_bindings(
    Sys.sleep = function(...) NULL,
    .package = "base"
  )
  local_silence_cli()

  result <- expand_and_search_detailed(
    author = c("A", "B"),
    thesis_type = "all",
    access_type = "all"
  )

  expect_s3_class(result, "tbl_df")
  expect_identical(nrow(result), 0L)
})

test_that("expand_and_search_detailed trims results to max_search_results", {
  state <- new.env(parent = emptyenv())
  state$call_count <- 0L
  testthat::local_mocked_bindings(
    search_detailed = function(...) {
      state$call_count <- state$call_count + 1L
      tibble::tibble(
        thesis_no = as.character(seq_len(50) + (state$call_count * 1000L)),
        title_original = rep("X", 50)
      )
    }
  )
  testthat::local_mocked_bindings(
    Sys.sleep = function(...) NULL,
    .package = "base"
  )
  local_silence_cli()

  result <- expand_and_search_detailed(
    author = c("A", "B", "C"),
    thesis_type = "all",
    access_type = "all",
    max_search_results = 10
  )

  expect_identical(nrow(result), 10L)
})

test_that("expand_and_search_detailed warns when results are trimmed", {
  testthat::local_mocked_bindings(
    search_detailed = function(...) {
      tibble::tibble(
        thesis_no = as.character(seq_len(50) + sample.int(10000, 1)),
        title_original = rep("X", 50)
      )
    }
  )
  testthat::local_mocked_bindings(
    Sys.sleep = function(...) NULL,
    .package = "base"
  )

  state <- new.env(parent = emptyenv())
  state$warnings_seen <- character()
  testthat::local_mocked_bindings(
    cli_alert_warning = function(text, ...) {
      state$warnings_seen <- c(
        state$warnings_seen,
        paste(text, collapse = " ")
      )
      invisible(NULL)
    },
    .package = "cli"
  )

  result <- expand_and_search_detailed(
    author = c("A", "B", "C"),
    thesis_type = "all",
    access_type = "all",
    max_search_results = 10
  )

  expect_identical(nrow(result), 10L)
  expect_true(any(grepl("Combined", state$warnings_seen, fixed = TRUE)))
  expect_true(any(grepl(
    "returning",
    state$warnings_seen,
    ignore.case = TRUE
  )))
  expect_true(any(grepl(
    "max_search_results",
    state$warnings_seen,
    fixed = TRUE
  )))
})

test_that("expand_and_search_detailed builds multi-value grid", {
  state <- new.env(parent = emptyenv())
  state$search_calls <- list()

  testthat::local_mocked_bindings(
    search_detailed = function(...) {
      args <- list(...)
      state$search_calls[[length(state$search_calls) + 1]] <- args
      tibble::tibble(thesis_no = character(), title_original = character())
    }
  )
  testthat::local_mocked_bindings(
    Sys.sleep = function(...) NULL,
    .package = "base"
  )
  local_silence_cli()

  expand_and_search_detailed(
    author = c("X", "Y"),
    thesis_type = c("phd", "masters"),
    access_type = "all"
  )

  # 2 authors x 2 thesis_types = 4 search calls
  expect_length(state$search_calls, 4L)
})
