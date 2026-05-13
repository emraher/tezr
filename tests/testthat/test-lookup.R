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

test_that("generic_lookup_item returns NULL for NULL input", {
  fetch_fn <- function() tibble::tibble(name = "X", id = "1", clean_name = "x")
  result <- generic_lookup_item(NULL, fetch_fn)
  expect_null(result)
})

test_that("generic_lookup_item returns NULL for empty string input", {
  fetch_fn <- function() tibble::tibble(name = "X", id = "1", clean_name = "x")
  result <- generic_lookup_item("", fetch_fn)
  expect_null(result)
})

test_that("generic_lookup_item returns first item on exact match", {
  items <- tibble::tibble(
    name = c("Ankara Uni", "Istanbul Uni"),
    id = c("3", "5"),
    clean_name = c("ankara uni", "istanbul uni")
  )
  fetch_fn <- function() items

  result <- generic_lookup_item("Ankara Uni", fetch_fn)
  expect_equal(result$id, "3")
  expect_equal(result$name, "Ankara Uni")
})

test_that("generic_lookup_item returns first item on substring match", {
  items <- tibble::tibble(
    name = c("Ankara Universitesi", "Istanbul Universitesi"),
    id = c("3", "5"),
    clean_name = c("ankara universitesi", "istanbul universitesi")
  )
  fetch_fn <- function() items

  result <- generic_lookup_item("ankara", fetch_fn)
  expect_equal(result$id, "3")
  expect_equal(result$name, "Ankara Universitesi")
})

test_that("generic_lookup_item returns NULL when no match found", {
  items <- tibble::tibble(
    name = c("Ankara Uni"),
    id = c("3"),
    clean_name = c("ankara uni")
  )
  fetch_fn <- function() items

  result <- generic_lookup_item("Nonexistent", fetch_fn)
  expect_null(result)
})

test_that("generic_lookup_item matches case-insensitively", {
  items <- tibble::tibble(
    name = c("ANKARA UNI", "Istanbul Uni"),
    id = c("3", "5"),
    clean_name = c("ankara uni", "istanbul uni")
  )
  fetch_fn <- function() items

  result <- generic_lookup_item("ankara uni", fetch_fn)
  expect_equal(result$id, "3")

  result2 <- generic_lookup_item("ANKARA UNI", fetch_fn)
  expect_equal(result2$id, "3")
})

test_that("generic_lookup_item matches when input case matches data case", {
  # Turkish İ (dotted capital I) lowercased produces i + combining dot.
  # Lookup works when both sides go through the same clean_text + str_to_lower path.
  all_caps <- "\u0130STANBUL \u00dcN\u0130VERS\u0130TES\u0130"
  items <- tibble::tibble(
    name = all_caps,
    id = "7",
    clean_name = stringr::str_to_lower(clean_text(all_caps))
  )
  fetch_fn <- function() items

  # Same all-caps input matches because both sides produce identical lowercased form
  result <- generic_lookup_item(all_caps, fetch_fn)
  expect_equal(result$id, "7")
})

test_that("generic_lookup_item falls back to substring for Turkish mixed-case", {
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
  result <- generic_lookup_item(mixed_case, fetch_fn)
  expect_equal(result$id, "7")
})

test_that("generic_lookup_item ignores Turkish case, accents, and spacing", {
  items <- tibble::tibble(
    name = c(
      "ANKARA ÜNİVERSİTESİ",
      "BÖLGESEL KALKINMA İKTİSAT ANABİLİM DALI",
      "İKTİSAT ANABİLİM DALI"
    ),
    id = c("3", "1753", "51"),
    clean_name = stringr::str_to_lower(clean_text(name))
  )
  fetch_fn <- function() items

  expect_equal(generic_lookup_item("Ankara Üniversitesi", fetch_fn)$id, "3")
  expect_equal(generic_lookup_item("İktisat Ana Bilim Dalı", fetch_fn)$id, "51")
})

test_that("generic_lookup_item returns canonical label and ID", {
  items <- tibble::tibble(
    name = c("ANKARA ÜNİVERSİTESİ", "İKTİSAT ANABİLİM DALI"),
    id = c("3", "51"),
    clean_name = stringr::str_to_lower(clean_text(name))
  )
  fetch_fn <- function() items

  matched_item <- generic_lookup_item("Ankara Üniversitesi", fetch_fn)

  expect_equal(matched_item$name, "ANKARA ÜNİVERSİTESİ")
  expect_equal(matched_item$id, "3")
})

test_that("generic_lookup_item prefers exact match over substring", {
  items <- tibble::tibble(
    name = c("Ankara Universitesi Fen", "Ankara Universitesi"),
    id = c("10", "20"),
    clean_name = c("ankara universitesi fen", "ankara universitesi")
  )
  fetch_fn <- function() items

  # Exact match should return "20", not "10" (which would match as substring)
  result <- generic_lookup_item("Ankara Universitesi", fetch_fn)
  expect_equal(result$id, "20")
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
  expect_equal(nrow(result), 0)
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

  expect_equal(result, cached_data)
})

test_that("generic_fetch_list parses eklecikar links from HTML", {
  local_clean_lookup_cache()

  html_content <- paste0(
    "<html><body>",
    "<a href=\"javascript:eklecikar('Test University','123')\">Test University</a>",
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
  expect_equal(nrow(result), 2)
  expect_equal(result$name, c("Test University", "Another Uni"))
  expect_equal(result$id, c("123", "456"))
  expect_true("clean_name" %in% names(result))
})

test_that("generic_fetch_list caches result and reuses on second call", {
  local_clean_lookup_cache()

  network_calls <- 0L
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
      network_calls <<- network_calls + 1L
      structure(list(), class = "httr2_response")
    },
    resp_body_html = function(resp, ...) fake_html,
    .package = "httr2"
  )

  # First call fetches from network
  result1 <- generic_fetch_list("test_cache_reuse", "test.jsp")
  expect_equal(network_calls, 1L)

  # Second call should use cache, not network
  result2 <- generic_fetch_list("test_cache_reuse", "test.jsp")
  expect_equal(network_calls, 1L)

  expect_equal(result1, result2)
})
