# Tests for recent thesis listing helpers (recent.R)

mock_recent_results_html <- '
<!DOCTYPE html>
<html>
<body>
<div class="result-limit-warning">
Tarama sonucunda 12 kayıt bulundu. 12 tanesi görüntülenmektedir.
</div>
<div class="result-card" data-kayitno="recent123" data-tezno="recent456" data-index="0">
  <div class="card-title">Recent thesis title</div>
  <div class="card-info"><strong>Tez No:</strong> 1004001</div>
</div>
<script>
const referenceData = {
  "0": {"meta": {"author": "RECENT AUTHOR", "year": "2026", "subject": "Ekonomi=Economics", "type": "Doktora", "lang": "Turkce", "yer": "TEST UNIVERSITESI / "}}
};
</script>
</body>
</html>
'

test_that("list_recent_theses validates mode", {
  expect_error(list_recent_theses(mode = "bad"), "must be one of")
})

test_that("list_recent_theses validates max_search_results before requests", {
  expect_error(
    list_recent_theses(max_search_results = -1),
    "max_search_results"
  )
})

test_that("list_recent_theses fetches TezIslemleri recent lists", {
  captured_url <- NULL

  testthat::local_mocked_bindings(
    has_session = function() TRUE,
    refresh_session_if_needed = function() invisible(NULL),
    increment_request_count = function() invisible(NULL),
    create_session = function(...) httr2::request("https://example.com"),
    .package = "tezr"
  )
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      captured_url <<- req$url
      structure(list(status = 200L), class = "httr2_response")
    },
    resp_status = function(resp) resp$status,
    resp_body_html = function(...) rvest::read_html(mock_recent_results_html),
    .package = "httr2"
  )

  listed_theses <- list_recent_theses(
    mode = "current_year",
    ignore_cache = TRUE
  )

  expect_s3_class(listed_theses, "tbl_df")
  expect_true(grepl(endpoints$recent, captured_url, fixed = TRUE))
  expect_true(grepl("islem=8", captured_url, fixed = TRUE))
  expect_equal(listed_theses$detail_id, "recent123")
  expect_equal(listed_theses$encrypted_no, "recent456")
  expect_equal(attr(listed_theses, "total_count", exact = TRUE), 12L)
})
