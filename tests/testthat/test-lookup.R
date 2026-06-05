# Tests for lookup functions (lookup.R)

# Helper: snapshot lookup_cache and restore on exit
local_clean_lookup_cache <- function(env = parent.frame()) {
  lookup_env <- get("lookup_cache", envir = asNamespace("tezr"))
  old_keys <- ls(lookup_env)
  old_values <- mget(old_keys, envir = lookup_env)

  withr::defer(
    {
      rm(list = ls(lookup_env), envir = lookup_env)
      for (k in names(old_values)) {
        lookup_env[[k]] <- old_values[[k]]
      }
    },
    envir = env
  )
}

test_that("lookup_cache is an environment", {
  expect_true(exists("lookup_cache", envir = asNamespace("tezr")))
})

test_that("generic_lookup_id returns NULL for NULL input", {
  fetch_fn <- function() tibble::tibble(name = "X", id = "1", clean_name = "x")
  result <- generic_lookup_id(NULL, fetch_fn)
  expect_null(result)
})

test_that("generic_lookup_id returns NULL for empty string input", {
  fetch_fn <- function() tibble::tibble(name = "X", id = "1", clean_name = "x")
  result <- generic_lookup_id("", fetch_fn)
  expect_null(result)
})

test_that("generic_lookup_id returns first ID on exact match", {
  items <- tibble::tibble(
    name = c("Ankara Uni", "Istanbul Uni"),
    id = c("3", "5"),
    clean_name = c("ankara uni", "istanbul uni")
  )
  fetch_fn <- function() items

  result <- generic_lookup_id("Ankara Uni", fetch_fn)
  expect_identical(result, "3")
})

test_that("generic_lookup_id returns first substring match", {
  items <- tibble::tibble(
    name = c("Ankara Universitesi", "Istanbul Universitesi"),
    id = c("3", "5"),
    clean_name = c("ankara universitesi", "istanbul universitesi")
  )
  fetch_fn <- function() items

  result <- generic_lookup_id("ankara", fetch_fn)
  expect_identical(result, "3")
})

test_that("generic_lookup_id returns NULL when no match found", {
  items <- tibble::tibble(
    name = "Ankara Uni",
    id = "3",
    clean_name = "ankara uni"
  )
  fetch_fn <- function() items

  result <- generic_lookup_id("Nonexistent", fetch_fn)
  expect_null(result)
})

test_that("generic_lookup_id matches case-insensitively", {
  items <- tibble::tibble(
    name = c("ANKARA UNI", "Istanbul Uni"),
    id = c("3", "5"),
    clean_name = c("ankara uni", "istanbul uni")
  )
  fetch_fn <- function() items

  result <- generic_lookup_id("ankara uni", fetch_fn)
  expect_identical(result, "3")

  result2 <- generic_lookup_id("ANKARA UNI", fetch_fn)
  expect_identical(result2, "3")
})

test_that("generic_lookup_id matches when input case matches data case", {
  # Turkish İ (dotted capital I) lowercased produces i + combining dot.
  # Lookup works when both sides use clean_text + str_to_lower.
  all_caps <- "\u0130STANBUL \u00dcN\u0130VERS\u0130TES\u0130"
  items <- tibble::tibble(
    name = all_caps,
    id = "7",
    clean_name = stringr::str_to_lower(clean_text(all_caps))
  )
  fetch_fn <- function() items

  # Same all-caps input matches because both sides lowercase identically.
  result <- generic_lookup_id(all_caps, fetch_fn)
  expect_identical(result, "7")
})

test_that("generic_lookup_id falls back to substring for Turkish mixed-case", {
  # When data has all-caps Turkish İ and user types mixed-case,
  # exact match fails due to Unicode normalization differences,

  # but substring match can still find a result if the clean input
  # is contained within the clean_name.
  all_caps <- "\u0130STANBUL \u00dcN\u0130VERS\u0130TES\u0130"
  mixed_case <- "\u0130stanbul"
  items <- tibble::tibble(
    name = all_caps,
    id = "7",
    clean_name = stringr::str_to_lower(clean_text(all_caps))
  )
  fetch_fn <- function() items

  # Substring of just the İ-prefixed word — str_to_lower("İstanbul") produces
  # "i̇stanbul" which IS contained in the clean_name "i̇stanbul ..."
  result <- generic_lookup_id(mixed_case, fetch_fn)
  expect_identical(result, "7")
})

test_that("generic_lookup_id prefers exact match over substring", {
  items <- tibble::tibble(
    name = c("Ankara Universitesi Fen", "Ankara Universitesi"),
    id = c("10", "20"),
    clean_name = c("ankara universitesi fen", "ankara universitesi")
  )
  fetch_fn <- function() items

  # Exact match should return "20", not "10" (which would match as substring)
  result <- generic_lookup_id("Ankara Universitesi", fetch_fn)
  expect_identical(result, "20")
})

test_that("generic_fetch_list returns empty tibble for no-link HTML", {
  local_clean_lookup_cache()

  fake_html <- rvest::read_html("<html><body>No links here</body></html>")

  testthat::local_mocked_bindings(
    create_session = function(...) structure(list(), class = "httr2_request"),
    .package = "tezr"
  )
  testthat::local_mocked_bindings(
    req_url = function(req, ...) req,
    req_perform = function(req, ...) {
      structure(list(), class = "httr2_response")
    },
    resp_body_html = function(resp, ...) fake_html,
    .package = "httr2"
  )

  result <- generic_fetch_list("test_empty", "test.jsp")

  expect_s3_class(result, "tbl_df")
  expect_identical(nrow(result), 0L)
  expect_true("name" %in% names(result))
  expect_true("id" %in% names(result))
})

test_that("generic_fetch_list returns cached result on second call", {
  local_clean_lookup_cache()

  lookup_env <- get("lookup_cache", envir = asNamespace("tezr"))

  cached_data <- tibble::tibble(
    name = "Cached Uni",
    id = "99",
    clean_name = "cached uni"
  )
  lookup_env[["test_cached"]] <- cached_data

  result <- generic_fetch_list("test_cached", "any.jsp")

  expect_identical(result, cached_data)
})

test_that("generic_fetch_list parses eklecikar links from HTML", {
  local_clean_lookup_cache()

  html_content <- paste0(
    "<html><body>",
    paste0(
      "<a href=\"javascript:eklecikar('Test University','123')\">",
      "Test University</a>"
    ),
    "<a href=\"javascript:eklecikar('Another Uni','456')\">Another Uni</a>",
    "</body></html>"
  )
  fake_html <- rvest::read_html(html_content)

  testthat::local_mocked_bindings(
    create_session = function(...) structure(list(), class = "httr2_request"),
    .package = "tezr"
  )
  testthat::local_mocked_bindings(
    req_url = function(req, ...) req,
    req_perform = function(req, ...) {
      structure(list(), class = "httr2_response")
    },
    resp_body_html = function(resp, ...) fake_html,
    .package = "httr2"
  )

  result <- generic_fetch_list("test_eklecikar", "test.jsp")

  expect_s3_class(result, "tbl_df")
  expect_identical(nrow(result), 2L)
  expect_identical(result$name, c("Test University", "Another Uni"))
  expect_identical(result$id, c("123", "456"))
  expect_true("clean_name" %in% names(result))
})

test_that("generic_fetch_list caches result and reuses on second call", {
  local_clean_lookup_cache()

  state <- new.env(parent = emptyenv())
  state$network_calls <- 0L
  html_content <- paste0(
    "<html><body>",
    "<a href=\"javascript:eklecikar('Uni A','1')\">Uni A</a>",
    "</body></html>"
  )
  fake_html <- rvest::read_html(html_content)

  testthat::local_mocked_bindings(
    create_session = function(...) structure(list(), class = "httr2_request"),
    .package = "tezr"
  )
  testthat::local_mocked_bindings(
    req_url = function(req, ...) req,
    req_perform = function(req, ...) {
      state$network_calls <- state$network_calls + 1L
      structure(list(), class = "httr2_response")
    },
    resp_body_html = function(resp, ...) fake_html,
    .package = "httr2"
  )

  # First call fetches from network
  result1 <- generic_fetch_list("test_cache_reuse", "test.jsp")
  expect_identical(state$network_calls, 1L)

  # Second call should use cache, not network
  result2 <- generic_fetch_list("test_cache_reuse", "test.jsp")
  expect_identical(state$network_calls, 1L)

  expect_identical(result1, result2)
})

test_that("fetch list wrappers use the configured endpoints", {
  state <- new.env(parent = emptyenv())
  state$calls <- list()

  testthat::with_mocked_bindings(
    {
      fetch_university_list()
      fetch_institute_list()
      fetch_division_list()
      fetch_discipline_list()
      fetch_subject_list()
    },
    generic_fetch_list = function(cache_key, endpoint) {
      state$calls[[length(state$calls) + 1L]] <- c(cache_key, endpoint)
      tibble::tibble(name = character(), id = character())
    },
    .package = "tezr"
  )

  expect_identical(
    state$calls,
    list(
      c("university", "uniEkle.jsp"),
      c("institute", "ensEkle.jsp"),
      c("division", "abdEkle.jsp"),
      c("discipline", "bilimDaliEkle.jsp"),
      c("subject", "konEkle.jsp")
    )
  )
})

test_that("lookup ID wrappers use the correct fetchers", {
  state <- new.env(parent = emptyenv())
  state$fetcher_names <- character()
  lookup_fetcher_name <- function(fetch_fn) {
    fetchers <- list(
      fetch_university_list = get("fetch_university_list", asNamespace("tezr")),
      fetch_institute_list = get("fetch_institute_list", asNamespace("tezr")),
      fetch_division_list = get("fetch_division_list", asNamespace("tezr")),
      fetch_discipline_list = get("fetch_discipline_list", asNamespace("tezr")),
      fetch_subject_list = get("fetch_subject_list", asNamespace("tezr"))
    )

    names(Filter(function(candidate) identical(candidate, fetch_fn), fetchers))
  }

  testthat::with_mocked_bindings(
    {
      expect_identical(lookup_university_id("a"), "a_id")
      expect_identical(lookup_institute_id("b"), "b_id")
      expect_identical(lookup_division_id("c"), "c_id")
      expect_identical(lookup_discipline_id("d"), "d_id")
      expect_identical(lookup_subject_id("e"), "e_id")
    },
    generic_lookup_id = function(name, fetch_fn) {
      state$fetcher_names <- c(
        state$fetcher_names,
        lookup_fetcher_name(fetch_fn)
      )
      paste0(name, "_id")
    },
    .package = "tezr"
  )

  expect_identical(
    state$fetcher_names,
    c(
      "fetch_university_list",
      "fetch_institute_list",
      "fetch_division_list",
      "fetch_discipline_list",
      "fetch_subject_list"
    )
  )
})

test_that("public list wrappers return cleaned lookup values", {
  lookup_values <- tibble::tibble(
    name = c("  Ankara  ", NA_character_, "Istanbul"),
    id = c("1", "2", "3")
  )

  result <- testthat::with_mocked_bindings(
    list_universities(),
    fetch_university_list = function() lookup_values,
    .package = "tezr"
  )

  expect_identical(result$name, c("Ankara", "Istanbul"))
  expect_identical(result$id, c("1", "3"))
})

test_that("public lookup lists delegate to list_lookup_values", {
  state <- new.env(parent = emptyenv())
  state$fetcher_names <- character()
  list_fetcher_name <- function(fetch_fn) {
    fetchers <- list(
      fetch_institute_list = get("fetch_institute_list", asNamespace("tezr")),
      fetch_division_list = get("fetch_division_list", asNamespace("tezr")),
      fetch_discipline_list = get("fetch_discipline_list", asNamespace("tezr"))
    )

    names(Filter(function(candidate) identical(candidate, fetch_fn), fetchers))
  }

  testthat::with_mocked_bindings(
    {
      list_institutes()
      list_divisions()
      list_disciplines()
    },
    list_lookup_values = function(fetch_fn) {
      state$fetcher_names <- c(
        state$fetcher_names,
        list_fetcher_name(fetch_fn)
      )
      tibble::tibble(name = "x", id = "1")
    },
    .package = "tezr"
  )

  expect_identical(
    state$fetcher_names,
    c("fetch_institute_list", "fetch_division_list", "fetch_discipline_list")
  )
})

test_that("list_subjects splits bilingual subject labels", {
  result <- testthat::with_mocked_bindings(
    list_subjects(),
    fetch_subject_list = function() {
      tibble::tibble(
        name = " Ekonomi = Economics ",
        id = "35"
      )
    },
    .package = "tezr"
  )

  expect_identical(result$name_tr, "Ekonomi")
  expect_identical(result$name_en, "Economics")
  expect_identical(result$id, "35")
})
