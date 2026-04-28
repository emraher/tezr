# Fixture tests using real HTML saved from tez.yok.gov.tr
#
# These tests parse real HTML pages to catch regressions in the scraper.
# Fixtures are from a search for "ekonometri" (subject) and detail page
# for thesis 393353 (Işıl Şirin Selçuk Çakmak, Ankara Üniversitesi, 2015).

# -- Search results (Turkish) -------------------------------------------------

test_that("parse_results_table extracts records from real search HTML", {
  html <- rvest::read_html(
    testthat::test_path("fixtures", "results_tr.html")
  )
  results <- parse_results_table(html)

  expect_s3_class(results, "tbl_df")
  expect_equal(nrow(results), 2000)

  # Check first record
  first <- results[1, ]
  expect_equal(first$thesis_no, "967755")
  expect_equal(first$author, "PERİHAN EZGİ BALLI")
  expect_equal(first$year, 2025L)
  expect_equal(first$university, "Bandırma Onyedi Eylül Üniversitesi")
  expect_equal(first$thesis_type_tr, "Doktora")
  expect_equal(first$language_tr, "Türkçe")

  # Bilingual title split

  expect_false(is.na(first$title_original))
  expect_false(is.na(first$title_translation))

  # Bilingual subject split
  expect_true(grepl("Ekonometri", first$subject_tr))
  expect_true(grepl("Econometrics", first$subject_en))

  # detail_id is present and non-empty
  expect_false(is.na(first$detail_id))
  expect_true(nchar(first$detail_id) > 0)

  # All rows have thesis_no
  expect_true(all(!is.na(results$thesis_no)))
})

test_that("extract_total_count works with real search HTML", {
  html <- rvest::read_html(
    testthat::test_path("fixtures", "results_tr.html")
  )
  count <- extract_total_count(html)
  expect_equal(count, 3697L)
})

# -- Detail page (Turkish) ----------------------------------------------------

test_that("parse_detail_page extracts fields from real detail HTML", {
  html <- rvest::read_html(
    testthat::test_path("fixtures", "detail_tr.html")
  )
  details <- parse_detail_page(html)

  expect_type(details, "list")
  expect_equal(details$thesis_no, "393353")
  expect_true(grepl("IŞIL ŞİRİN", details$author))
  expect_equal(details$year, "2015")
  expect_equal(details$pages, "153")
  expect_equal(details$access_status, "open")
})

test_that("parse_detail_page extracts advisor from real detail HTML", {
  html <- rvest::read_html(
    testthat::test_path("fixtures", "detail_tr.html")
  )
  details <- parse_detail_page(html)

  expect_true(grepl("HASAN ŞAHİN", details$advisor))
})

test_that("parse_detail_page extracts location from real detail HTML", {
  html <- rvest::read_html(
    testthat::test_path("fixtures", "detail_tr.html")
  )
  details <- parse_detail_page(html)

  expect_true(grepl("Ankara", details$university))
  expect_true(grepl("Sosyal Bilimler", details$institute))
  expect_true(grepl("ktisat", details$division))
})

test_that("parse_detail_page extracts subjects from real detail HTML", {
  html <- rvest::read_html(
    testthat::test_path("fixtures", "detail_tr.html")
  )
  details <- parse_detail_page(html)

  expect_false(is.na(details$subject_tr))
  expect_true(grepl("Ekonomi", details$subject_tr))
  expect_true(grepl("Economics", details$subject_en))
})

test_that("parse_detail_page extracts abstracts from real detail HTML", {
  html <- rvest::read_html(
    testthat::test_path("fixtures", "detail_tr.html")
  )
  details <- parse_detail_page(html)

  expect_false(is.na(details$abstract_original))
  expect_false(is.na(details$abstract_translation))
  expect_true(grepl(
    "enerji piyas",
    details$abstract_original,
    ignore.case = TRUE
  ))
  expect_true(grepl(
    "energy market",
    details$abstract_translation,
    ignore.case = TRUE
  ))
})

test_that("parse_detail_page extracts PDF URL from real detail HTML", {
  html <- rvest::read_html(
    testthat::test_path("fixtures", "detail_tr.html")
  )
  details <- parse_detail_page(html)

  expect_false(is.na(details$pdf_url))
  expect_true(grepl("TezGoster", details$pdf_url))
})

# -- Detail page (English) ----------------------------------------------------
# The parser targets Turkish-language pages. English detail pages use different
# column headers and field labels, so most fields return NA. Abstracts are
# extracted because they use positional ids (td0, td1) rather than labels.

test_that("parse_detail_page extracts abstracts from English detail HTML", {
  html <- rvest::read_html(
    testthat::test_path("fixtures", "detail_en.html")
  )
  details <- parse_detail_page(html)

  expect_type(details, "list")
  expect_false(is.na(details$abstract_original))
  expect_false(is.na(details$abstract_translation))
  expect_true(grepl(
    "energy market",
    details$abstract_translation,
    ignore.case = TRUE
  ))
})
