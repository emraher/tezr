# Live integration tests for tezr
#
# These tests hit the real YOK server and verify the scraper still works.
# Skipped by default. Run with:
#   TEZR_LIVE_TESTS=true Rscript -e
#   'testthat::test_file("tests/testthat/test-integration.R")'

skip_if(
  Sys.getenv("TEZR_LIVE_TESTS") != "true",
  "Live integration tests disabled. Set TEZR_LIVE_TESTS=true to run."
)

test_that("basic search returns results", {
  results <- search_basic("ekonometri", search_field = "title")
  expect_s3_class(results, "tbl_df")
  expect_gt(nrow(results), 0)
  expect_true("thesis_no" %in% names(results))
  expect_true("detail_id" %in% names(results))
})

test_that("detailed search with university filter works", {
  results <- search_detailed(
    subject = "Ekonometri",
    year_start = 2020,
    year_end = 2022
  )
  expect_s3_class(results, "tbl_df")
  expect_gt(nrow(results), 0)
})

test_that("detail retrieval works for a single thesis", {
  results <- search_basic("ekonometri", search_field = "title")
  skip_if(nrow(results) == 0, "No search results to fetch details for")

  thesis_details <- detail(results$detail_id[1])
  expect_s3_class(thesis_details, "tbl_df")
  expect_identical(nrow(thesis_details), 1L)
  expect_true("thesis_no" %in% names(thesis_details))
  expect_true("abstract_original" %in% names(thesis_details))
})

test_that("statistics functions return data", {
  uni_stats <- stats_universities()
  expect_s3_class(uni_stats, "tbl_df")
  expect_gt(nrow(uni_stats), 0)
})

test_that("lookup functions return lists", {
  universities <- list_universities()
  expect_s3_class(universities, "tbl_df")
  expect_gt(nrow(universities), 0)
  expect_true("name" %in% names(universities))
  expect_true("id" %in% names(universities))
})
