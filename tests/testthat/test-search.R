# Tests for search functions (search.R)

test_that("search_basic validates query input", {
  expect_error(search_basic(""), "non-empty")
  expect_error(search_basic("   "), "non-empty")
  expect_error(search_basic(NA_character_), "non-empty")
  expect_error(search_basic(123), "single non-empty character string")
  expect_error(search_basic(c("a", "b")), "single non-empty character string")
})

test_that("search_basic validates enum parameters", {
  expect_error(search_basic("test", search_field = "invalid"), "must be one of")
  expect_error(search_basic("test", thesis_type = "invalid"), "must be one of")
  expect_error(search_basic("test", access_type = "invalid"), "must be one of")
})

test_that("search functions validate max_search_results before requests", {
  expect_error(
    search_basic("test", max_search_results = -1),
    "max_search_results"
  )
  expect_error(
    search_advanced("test", max_search_results = 0),
    "max_search_results"
  )
  expect_error(
    search_detailed(title = "test", max_search_results = "many"),
    "max_search_results"
  )
})

test_that("search_basic points thesis number searches to detailed search", {
  expect_error(
    search_basic("1003627", search_field = "thesis_no"),
    "search_detailed\\(thesis_no"
  )
})

test_that("search_basic returns cached results without network", {
  cached <- tibble::tibble(thesis_no = "1")

  testthat::local_mocked_bindings(
    run_basic_search = function(...) list(results = cached, total_count = 1L)
  )

  result <- search_basic("test")
  expect_equal(names(result), names(cached))
  expect_equal(result$thesis_no, cached$thesis_no)
  expect_equal(attr(result, "total_count", exact = TRUE), 1L)
  expect_false(attr(result, "paginated", exact = TRUE))
  expect_true(attr(result, "complete", exact = TRUE))
})

test_that("search_basic uses shared basic search helper", {
  called <- FALSE
  fake <- function(...) {
    called <<- TRUE
    list(results = tibble::tibble(), total_count = 0L)
  }

  testthat::local_mocked_bindings(
    run_basic_search = fake
  )

  search_basic("test")
  expect_true(called)
})

test_that("search_basic forwards ignore_cache to run_basic_search", {
  captured_ignore_cache <- NULL

  testthat::local_mocked_bindings(
    run_basic_search = function(
      form_data,
      cache_key,
      cache_label,
      ignore_cache = FALSE
    ) {
      captured_ignore_cache <<- ignore_cache
      list(results = empty_results_tibble(), total_count = 0L)
    }
  )

  search_basic("test", ignore_cache = TRUE)
  expect_true(isTRUE(captured_ignore_cache))
})

test_that("search_advanced validates keyword input", {
  expect_error(search_advanced(keyword = ""), "non-empty")
  expect_error(search_advanced(keyword = "   "), "non-empty")
  expect_error(search_advanced(keyword = NA_character_), "non-empty")
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

test_that("search_advanced validates language input before requests", {
  expect_error(search_advanced("test", language = ""), "language")
  expect_error(search_advanced("test", language = NA_character_), "language")
  expect_error(search_advanced("test", language = c("tr", "en")), "language")
})

test_that("search_advanced returns cached results without network", {
  cached <- tibble::tibble(thesis_no = "1")

  testthat::local_mocked_bindings(
    get_cached = function(...) cached
  )

  result <- search_advanced("test")
  expect_equal(result, cached)
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
  expect_equal(nrow(result), 1)
})

test_that("search_advanced paginates when year range provided", {
  called <- FALSE

  testthat::local_mocked_bindings(
    perform_search_request = function(...) {
      list(
        total_count = 3000L,
        search_results = tibble::tibble(thesis_no = "1")
      )
    },
    fetch_by_year_ranges = function(...) {
      called <<- TRUE
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

  expect_true(called)
})

test_that("search_advanced forwards university and group filters", {
  captured <- new.env(parent = emptyenv())
  captured$form <- NULL

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) NULL,
    init_session = function(...) NULL,
    lookup_university_item = function(name) {
      expect_equal(name, "Test Uni")
      tibble::tibble(name = "TEST UNI", id = "123")
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
    group = "science",
    university = "Test Uni"
  )

  expect_equal(nrow(result), 0)
  expect_equal(captured$form$islem, 4L)
  expect_equal(captured$form$EnstituGrubu, group_codes$science)
  expect_equal(captured$form$Universite, 123L)
  expect_equal(captured$form$source, "TR")
})

test_that("search_advanced routes institute filters through detailed form", {
  captured <- new.env(parent = emptyenv())
  captured$form <- NULL

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) NULL,
    init_session = function(...) NULL,
    lookup_institute_item = function(name) {
      expect_equal(name, "Test Inst")
      tibble::tibble(name = "TEST INST", id = "215")
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
    search_field = "title",
    match_type = "contains",
    institute = "Test Inst"
  )

  expect_equal(nrow(result), 0)
  expect_equal(captured$form$islem, 2L)
  expect_equal(captured$form$TezAd, "test")
  expect_equal(captured$form$ensad, "TEST INST")
  expect_equal(captured$form$Enstitu, 215L)
  expect_equal(captured$form$selected_institute, "on")
})

test_that("search_advanced rejects all-field institute filters", {
  expect_error(
    search_advanced(
      keyword = "test",
      search_field = "all",
      institute_id = 215L
    ),
    "all-field keyword endpoint ignores institute"
  )
})

test_that("search_advanced defaults to title field to match web advanced form", {
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

  expect_equal(nrow(result), 0)
  expect_equal(captured$form$nevi, search_field_codes$title)
  expect_equal(captured$form$tip, match_type_codes$exact)
  expect_equal(captured$form$Tur, thesis_type_codes$all)
  expect_equal(captured$form$Dil, 0L)
  expect_equal(captured$form$izin, access_type_codes$all)
  expect_equal(captured$form$Durum, status_codes$approved)
})

test_that("search_advanced with Inf reports single-year overflow instead of Inf hint", {
  warnings_seen <- character()
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
      warnings_seen <<- c(warnings_seen, paste(text, collapse = " "))
      invisible(NULL)
    },
    .package = "cli"
  )

  result <- search_advanced(
    keyword = "avrupa",
    search_field = "all",
    max_search_results = Inf
  )

  expect_equal(nrow(result), 28584)
  expect_true(any(grepl("single-year", warnings_seen, ignore.case = TRUE)))
  expect_true(any(grepl("2019", warnings_seen)))
  expect_false(any(grepl(
    "max_search_results = Inf",
    warnings_seen,
    fixed = TRUE
  )))
})

test_that("search_advanced with Inf bypasses cached capped results", {
  cache_store <- new.env(parent = emptyenv())
  paginated <- FALSE
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
      paginated <<- TRUE
      paginated_results
    }
  )

  first <- search_advanced(keyword = "education")
  second <- search_advanced(keyword = "education", max_search_results = Inf)

  expect_equal(nrow(first), 2000)
  expect_true(paginated)
  expect_equal(nrow(second), 3000)
})

test_that("build_search_cache_key matches previous behavior", {
  key_old <- make_search_key(type = "basic", query = "x", field = "all")
  key_new <- build_search_cache_key(
    type = "basic",
    params = list(query = "x", field = "all")
  )

  expect_equal(key_new, key_old)
})

test_that("search_basic does not hit network when run_basic_search mocked", {
  testthat::local_mocked_bindings(
    run_basic_search = function(...) {
      list(results = empty_results_tibble(), total_count = 0L)
    }
  )

  result <- search_basic("test")
  expect_equal(nrow(result), 0)
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

test_that("search_basic trims server-visible results to max_search_results", {
  fake_results <- tibble::tibble(thesis_no = as.character(seq_len(10)))

  testthat::local_mocked_bindings(
    run_basic_search = function(...) {
      list(results = fake_results, total_count = 10L)
    }
  )

  expect_message(
    result <- search_basic("test", max_search_results = 3),
    "Returning 3"
  )

  expect_equal(nrow(result), 3L)
  expect_equal(result$thesis_no, c("1", "2", "3"))
  expect_equal(attr(result, "total_count", exact = TRUE), 10L)
})

test_that("search_basic delegates to advanced search when max_search_results > 2000", {
  delegated <- FALSE
  advanced_args <- list()
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
      delegated <<- TRUE
      advanced_args <<- list(
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

  expect_true(delegated)
  expect_equal(advanced_args$keyword, "test query")
  expect_equal(advanced_args$search_field, "title")
  expect_equal(advanced_args$thesis_type, "phd")
  expect_equal(advanced_args$match_type, "contains")
  expect_equal(advanced_args$access_type, "open")
  expect_equal(nrow(result), 3000)
})

test_that("search_basic to search_advanced delegation reuses one initialized session", {
  old_cookies <- tezr_env$cookies
  old_session_start <- tezr_env$session_start
  old_request_count <- tezr_env$request_count

  withr::defer({
    tezr_env$cookies <- old_cookies
    tezr_env$session_start <- old_session_start
    tezr_env$request_count <- old_request_count
  })

  tezr_env$cookies <- NULL
  tezr_env$session_start <- NULL
  tezr_env$request_count <- 0L

  init_calls <- 0L
  request_calls <- 0L

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) NULL,
    init_session = function(...) {
      init_calls <<- init_calls + 1L
      tezr_env$cookies <- "JSESSIONID=test"
      tezr_env$session_start <- Sys.time()
      tezr_env$request_count <- 0L
      invisible(TRUE)
    },
    perform_search_request = function(...) {
      request_calls <<- request_calls + 1L
      if (request_calls == 1L) {
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

  expect_equal(init_calls, 1L)
  expect_equal(nrow(result), 10L)
})

test_that("search_basic does not delegate when results fit in 2000", {
  fake_results <- tibble::tibble(thesis_no = as.character(seq_len(500)))

  testthat::local_mocked_bindings(
    run_basic_search = function(...) {
      list(results = fake_results, total_count = 500L)
    }
  )

  result <- search_basic("test", max_search_results = Inf)
  expect_equal(nrow(result), 500)
})

test_that("search_basic with Inf delegates even after truncated basic result is cached", {
  cache_store <- new.env(parent = emptyenv())
  delegated <- FALSE
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
      delegated <<- TRUE
      advanced_results
    }
  )

  first <- search_basic(keyword = "climate change")
  second <- search_basic(keyword = "climate change", max_search_results = Inf)

  expect_equal(nrow(first), 2000)
  expect_true(delegated)
  expect_equal(nrow(second), 3000)
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

  expect_equal(result, expected)
  expect_equal(called$args$thesis_type, c("phd", "masters"))
})

test_that("search_detailed expansion does not apply combined cap when max_search_results is omitted", {
  captured_limit_flag <- NULL
  expected <- tibble::tibble(thesis_no = as.character(seq_len(4036)))

  testthat::local_mocked_bindings(
    expand_and_search_detailed = function(..., limit_combined_results = TRUE) {
      captured_limit_flag <<- limit_combined_results
      expected
    }
  )

  result <- search_detailed(subject = c("Ekonomi", "Iktisat"))

  expect_equal(nrow(result), 4036)
  expect_false(isTRUE(captured_limit_flag))
})

test_that("search_detailed trims cached results to max_search_results", {
  cached <- tibble::tibble(thesis_no = c("1", "2", "3"))

  testthat::local_mocked_bindings(
    get_cached = function(...) cached
  )

  result <- search_detailed(author = "test", max_search_results = 2)

  expect_equal(nrow(result), 2)
  expect_equal(result$thesis_no, c("1", "2"))
})

test_that("search_detailed with Inf bypasses cached capped results", {
  cache_store <- new.env(parent = emptyenv())
  paginated <- FALSE
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
    perform_search_request = function(...) {
      list(
        total_count = 5000L,
        search_results = tibble::tibble(
          thesis_no = as.character(seq_len(2000))
        )
      )
    },
    fetch_by_year_ranges = function(...) {
      paginated <<- TRUE
      paginated_results
    }
  )

  first <- search_detailed(subject = "Economics")
  second <- search_detailed(subject = "Economics", max_search_results = Inf)

  expect_equal(nrow(first), 2000)
  expect_true(paginated)
  expect_equal(nrow(second), 3500)
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
  expect_equal(nrow(result), 1)
})

test_that("search_detailed validates status parameter", {
  expect_error(
    search_detailed(author = "test", status = "invalid"),
    "Invalid.*status"
  )
  expect_error(
    search_detailed(author = "test", status = c("approved", "all")),
    "status"
  )
})

test_that("search_detailed validates empty vector filters", {
  expect_error(
    search_detailed(author = "test", thesis_type = character(0)),
    "thesis_type"
  )
  expect_error(
    search_detailed(author = "test", access_type = character(0)),
    "access_type"
  )
  expect_error(
    search_detailed(author = "test", group = character(0)),
    "group"
  )
})

test_that("search_detailed validates blank text criteria", {
  expect_error(search_detailed(author = "   "), "author")
  expect_error(search_detailed(title = c("valid", "   ")), "title")
  expect_error(search_detailed(subject = NA_character_), "subject")
  expect_error(search_detailed(university = character(0)), "university")
})

test_that("search_detailed validates language values before expansion", {
  expect_error(search_detailed(author = "test", language = ""), "language")
  expect_error(search_detailed(author = "test", language = NA_character_), "language")
  expect_error(search_detailed(author = "test", language = c("tr", "")), "language")
})

test_that("search_detailed sends field and institutional filters to detailed form", {
  captured <- new.env(parent = emptyenv())
  captured$form <- NULL

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) NULL,
    has_session = function(...) TRUE,
    init_session = function(...) NULL,
    perform_search_request = function(form_data) {
      captured$form <- form_data
      list(
        total_count = 0L,
        search_results = tibble::tibble(thesis_no = character())
      )
    }
  )

  result <- search_detailed(
    title = "sulama",
    university_id = 25L,
    group = "science",
    year_start = 2015,
    year_end = 2024
  )

  expect_equal(nrow(result), 0)
  expect_equal(captured$form$islem, 2L)
  expect_equal(captured$form$TezAd, "sulama")
  expect_equal(captured$form$Universite, 25L)
  expect_equal(captured$form$EnstituGrubu, group_codes$science)
  expect_equal(captured$form$yil1, 2015L)
  expect_equal(captured$form$yil2, 2024L)
  expect_false("keyword" %in% names(captured$form))
  expect_false("nevi" %in% names(captured$form))
})

test_that("search_detailed posts canonical lookup labels from YOK lists", {
  captured <- new.env(parent = emptyenv())
  captured$form <- NULL

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) NULL,
    has_session = function(...) TRUE,
    init_session = function(...) NULL,
    lookup_university_item = function(name) {
      expect_equal(name, "Ankara Üniversitesi")
      tibble::tibble(name = "ANKARA ÜNİVERSİTESİ", id = "3")
    },
    lookup_division_item = function(name) {
      expect_equal(name, "İktisat Ana Bilim Dalı")
      tibble::tibble(name = "İKTİSAT ANABİLİM DALI", id = "51")
    },
    perform_search_request = function(form_data) {
      captured$form <- form_data
      list(
        total_count = 1L,
        search_results = tibble::tibble(thesis_no = "1")
      )
    }
  )

  result <- search_detailed(
    university = "Ankara Üniversitesi",
    division = "İktisat Ana Bilim Dalı",
    thesis_type = "phd",
    year_start = 2020
  )

  expect_equal(result$thesis_no, "1")
  expect_equal(captured$form$uniad, "ANKARA ÜNİVERSİTESİ")
  expect_equal(captured$form$Universite, 3L)
  expect_equal(captured$form$abdad, "İKTİSAT ANABİLİM DALI")
  expect_equal(captured$form$ABD, 51L)
})

test_that("search_detailed retries university filters locally when YOK returns zero", {
  forms <- list()
  fake_fallback_results <- tibble::tibble(
    thesis_no = c("955043", "900001"),
    university = c("ANKARA ÜNİVERSİTESİ", "İSTANBUL ÜNİVERSİTESİ")
  )

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) NULL,
    has_session = function(...) TRUE,
    init_session = function(...) NULL,
    lookup_university_item = function(name) {
      expect_equal(name, "Ankara Üniversitesi")
      tibble::tibble(name = "ANKARA ÜNİVERSİTESİ", id = "3")
    },
    lookup_division_item = function(name) {
      expect_equal(name, "İktisat Ana Bilim Dalı")
      tibble::tibble(name = "İKTİSAT ANABİLİM DALI", id = "51")
    },
    perform_search_request = function(form_data) {
      forms[[length(forms) + 1L]] <<- form_data
      if (!identical(form_data$Universite, "")) {
        return(list(
          total_count = 0L,
          search_results = tibble::tibble(thesis_no = character())
        ))
      }

      list(
        total_count = nrow(fake_fallback_results),
        search_results = fake_fallback_results
      )
    }
  )

  result <- search_detailed(
    university = "Ankara Üniversitesi",
    division = "İktisat Ana Bilim Dalı",
    thesis_type = "phd",
    year_start = 2020
  )

  expect_equal(length(forms), 2L)
  expect_equal(forms[[1]]$uniad, "ANKARA ÜNİVERSİTESİ")
  expect_equal(forms[[1]]$Universite, 3L)
  expect_equal(forms[[2]]$uniad, "")
  expect_equal(forms[[2]]$Universite, "")
  expect_equal(result$thesis_no, "955043")
  expect_equal(result$university, "ANKARA ÜNİVERSİTESİ")
  expect_true(attr(result, "complete", exact = TRUE))
})

test_that("search_detailed retries university filters with narrowed type-year queries", {
  forms <- list()
  fake_fallback_results <- tibble::tibble(
    thesis_no = c("955043", "900001"),
    university = c("ANKARA ÜNİVERSİTESİ", "İSTANBUL ÜNİVERSİTESİ")
  )

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) NULL,
    has_session = function(...) TRUE,
    init_session = function(...) NULL,
    lookup_university_item = function(name) {
      expect_equal(name, "Ankara Üniversitesi")
      tibble::tibble(name = "ANKARA ÜNİVERSİTESİ", id = "3")
    },
    perform_search_request = function(form_data) {
      forms[[length(forms) + 1L]] <<- form_data
      if (!identical(form_data$Universite, "")) {
        return(list(
          total_count = 0L,
          search_results = tibble::tibble(thesis_no = character())
        ))
      }

      list(
        total_count = nrow(fake_fallback_results),
        search_results = fake_fallback_results
      )
    }
  )

  result <- search_detailed(
    university = "Ankara Üniversitesi",
    thesis_type = "phd",
    year_start = 2020
  )

  expect_equal(length(forms), 2L)
  expect_equal(forms[[2]]$Tur, thesis_type_codes$phd)
  expect_equal(forms[[2]]$yil1, 2020L)
  expect_equal(result$thesis_no, "955043")
})

test_that("search_detailed sends subject searches through detailed form", {
  captured <- new.env(parent = emptyenv())
  captured$form <- NULL
  fake_results <- tibble::tibble(
    thesis_no = c("1", "2", "3"),
    subject_tr = c("Ekonometri", "Ekonomi; Ekonometri", "Ekonomi"),
    subject_en = c(NA_character_, NA_character_, NA_character_)
  )

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) NULL,
    has_session = function(...) TRUE,
    init_session = function(...) NULL,
    perform_search_request = function(form_data) {
      captured$form <- form_data
      list(
        total_count = 3L,
        search_results = fake_results
      )
    }
  )

  result <- search_detailed(
    subject = "Ekonometri",
    thesis_type = "phd",
    year_start = 2020
  )

  expect_equal(captured$form$islem, 2L)
  expect_equal(captured$form$Konu, "Ekonometri")
  expect_equal(captured$form$Tur, thesis_type_codes$phd)
  expect_equal(captured$form$yil1, 2020L)
  expect_equal(result$thesis_no, c("1", "2", "3"))
  expect_equal(attr(result, "total_count", exact = TRUE), 3L)
})

test_that("search_detailed uses detailed form for thesis number", {
  captured <- new.env(parent = emptyenv())
  captured$form <- NULL

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) NULL,
    has_session = function(...) TRUE,
    init_session = function(...) NULL,
    perform_search_request = function(form_data) {
      captured$form <- form_data
      list(
        total_count = 1L,
        search_results = tibble::tibble(thesis_no = "12345")
      )
    }
  )

  result <- search_detailed(thesis_no = "12345")

  expect_equal(result$thesis_no, "12345")
  expect_equal(captured$form$islem, 2L)
  expect_equal(captured$form$TezNo, "12345")
  expect_equal(captured$form$source, "TR")
  expect_equal(captured$form$uni_yoksis_id, "")
  expect_equal(captured$form$Universite, "")
  expect_false("keyword" %in% names(captured$form))
  expect_false("nevi" %in% names(captured$form))
})

test_that("search_detailed supports institute-only detailed searches", {
  captured <- new.env(parent = emptyenv())
  captured$form <- NULL

  testthat::local_mocked_bindings(
    get_cached = function(...) NULL,
    set_cached = function(...) NULL,
    has_session = function(...) TRUE,
    init_session = function(...) NULL,
    perform_search_request = function(form_data) {
      captured$form <- form_data
      list(
        total_count = 1L,
        search_results = tibble::tibble(thesis_no = "997244")
      )
    }
  )

  result <- search_detailed(institute_id = 215L)

  expect_equal(result$thesis_no, "997244")
  expect_equal(captured$form$islem, 2L)
  expect_equal(captured$form$Enstitu, 215L)
  expect_equal(captured$form$selected_institute, "on")
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
  call_count <- 0L
  fake_search <- function(...) {
    call_count <<- call_count + 1L
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
  expect_equal(nrow(result), 2)
  expect_equal(result$thesis_no, c("1", "2"))
})

test_that("expand_and_search_detailed returns empty tibble when all searches fail", {
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
  expect_equal(nrow(result), 0)
})

test_that("expand_and_search_detailed trims results to max_search_results", {
  call_count <- 0L
  testthat::local_mocked_bindings(
    search_detailed = function(...) {
      call_count <<- call_count + 1L
      tibble::tibble(
        thesis_no = as.character(seq_len(50) + (call_count * 1000L)),
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

  expect_equal(nrow(result), 10)
})

test_that("expand_and_search_detailed warns when combined results are trimmed", {
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

  warnings_seen <- character()
  testthat::local_mocked_bindings(
    cli_alert_warning = function(text, ...) {
      warnings_seen <<- c(warnings_seen, paste(text, collapse = " "))
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

  expect_equal(nrow(result), 10)
  expect_true(any(grepl("Combined", warnings_seen)))
  expect_true(any(grepl("returning", warnings_seen, ignore.case = TRUE)))
  expect_true(any(grepl("max_search_results", warnings_seen)))
})

test_that("expand_and_search_detailed creates correct grid for multi-value params", {
  search_calls <- list()

  testthat::local_mocked_bindings(
    search_detailed = function(...) {
      args <- list(...)
      search_calls[[length(search_calls) + 1]] <<- args
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
  expect_equal(length(search_calls), 4)
})
