# Tests for statistics functions (statistics.R)

# Helper: run fetch_and_parse_stats against a pre-built HTML document
with_fake_stats_html <- function(fake_html, code) {
  testthat::local_mocked_bindings(
    create_session = function(...) structure(list(), class = "httr2_request"),
    .package = "tezr"
  )
  testthat::local_mocked_bindings(
    req_url = function(req, ...) req,
    req_url_query = function(req, ...) req,
    req_perform = function(req, ...) {
      structure(list(status = 200L), class = "httr2_response")
    },
    resp_status = function(resp) 200L,
    resp_body_html = function(resp, ...) fake_html,
    .package = "httr2"
  )
  force(code)
}

# Mock HTML builders
mock_stats_html <- function(islem = 1) {
  if (islem == 1) {
    rvest::read_html(
      "
      <html><body>
      <table><tr><td>ignore</td></tr></table>
      <table>
        <tr><td colspan='5'>Tez Istatistikleri</td></tr>
        <tr>
          <td>Universite</td><td>YL</td><td>DR</td>
          <td>TU</td><td>Toplam</td>
        </tr>
        <tr>
          <td>Ankara Uni</td><td>100</td><td>50</td>
          <td>10</td><td>160</td>
        </tr>
        <tr>
          <td>Istanbul Uni</td><td>200</td><td>80</td>
          <td>20</td><td>300</td>
        </tr>
      </table>
      </body></html>
    "
    )
  } else if (islem == 2) {
    rvest::read_html(
      "
      <html><body>
      <table><tr><td>ignore</td></tr></table>
      <table>
        <tr><td colspan='5'>Tez Istatistikleri</td></tr>
        <tr><td>Yil</td><td>YL</td><td>DR</td><td>TU</td><td>Toplam</td></tr>
        <tr><td>2020</td><td>1000</td><td>500</td><td>100</td><td>1600</td></tr>
        <tr><td>2021</td><td>1100</td><td>550</td><td>110</td><td>1760</td></tr>
      </table>
      </body></html>
    "
    )
  } else if (islem == 3) {
    rvest::read_html(
      "
      <html><body>
      <table><tr><td>ignore</td></tr></table>
      <table>
        <tr><td colspan='6'>Tez Istatistikleri</td></tr>
        <tr>
          <td>Konu</td><td>X2</td><td>X3</td>
          <td>YL</td><td>DR</td><td>Toplam</td>
        </tr>
        <tr>
          <td>Fizik</td><td>a</td><td>b</td>
          <td>40</td><td>20</td><td>60</td>
        </tr>
        <tr>
          <td>Kimya</td><td>c</td><td>d</td>
          <td>30</td><td>10</td><td>40</td>
        </tr>
      </table>
      </body></html>
    "
    )
  } else if (islem == 4) {
    rvest::read_html(
      "
      <html><body>
      <table><tr><td>ignore</td></tr></table>
      <table>
        <tr><td colspan='2'>Tez Istatistikleri</td></tr>
        <tr><td>Tur</td><td>Toplam</td></tr>
        <tr><td>1</td><td>5000</td></tr>
        <tr><td>2</td><td>3000</td></tr>
      </table>
      </body></html>
    "
    )
  }
}

# ---- Error handling ----

test_that("fetch_and_parse_stats errors on non-200 response", {
  testthat::local_mocked_bindings(
    create_session = function(...) structure(list(), class = "httr2_request"),
    .package = "tezr"
  )
  testthat::local_mocked_bindings(
    req_url = function(req, ...) req,
    req_url_query = function(req, ...) req,
    req_perform = function(req, ...) {
      structure(list(status = 500L), class = "httr2_response")
    },
    resp_status = function(resp) resp$status,
    .package = "httr2"
  )

  expect_error(fetch_and_parse_stats(1), "Failed to fetch statistics")
})

test_that("fetch_and_parse_stats errors when fewer than 2 tables found", {
  one_table <- rvest::read_html(
    "<html><body><table><tr><td>only one</td></tr></table></body></html>"
  )
  with_fake_stats_html(one_table, {
    expect_error(fetch_and_parse_stats(1), "Could not find statistics table")
  })
})

test_that("fetch_and_parse_stats errors when zero tables found", {
  no_tables <- rvest::read_html(
    "<html><body><p>No tables here</p></body></html>"
  )
  with_fake_stats_html(no_tables, {
    expect_error(fetch_and_parse_stats(1), "Could not find statistics table")
  })
})

# ---- Empty / minimal tables ----

test_that("fetch_and_parse_stats returns empty tibble for header-only table", {
  header_only <- rvest::read_html(
    "
    <html><body>
    <table><tr><td>ignore</td></tr></table>
    <table>
      <tr><td colspan='3'>Title</td></tr>
      <tr><td>A</td><td>B</td><td>C</td></tr>
    </table>
    </body></html>
  "
  )
  with_fake_stats_html(header_only, {
    result <- fetch_and_parse_stats(1)
    expect_s3_class(result, "tbl_df")
    expect_identical(nrow(result), 0L)
  })
})

test_that("fetch_and_parse_stats returns empty tibble for all-NA data row", {
  all_blank <- rvest::read_html(
    "
    <html><body>
    <table><tr><td>ignore</td></tr></table>
    <table>
      <tr><td colspan='3'>Title</td></tr>
      <tr><td>Col1</td><td>Col2</td><td>Col3</td></tr>
      <tr><td>  </td><td>  </td><td>  </td></tr>
    </table>
    </body></html>
  "
  )
  with_fake_stats_html(all_blank, {
    result <- fetch_and_parse_stats(1)
    expect_s3_class(result, "tbl_df")
    expect_identical(nrow(result), 0L)
  })
})

test_that("clean_stats_rows keeps zero-column rows", {
  stats_rows <- tibble::new_tibble(list(), nrow = 2L)

  result <- clean_stats_rows(stats_rows)

  expect_s3_class(result, "tbl_df")
  expect_identical(nrow(result), 2L)
  expect_identical(ncol(result), 0L)
})

# ---- Column coercion ----

test_that("university stats coerces numeric columns to integer", {
  with_fake_stats_html(mock_stats_html(1), {
    result <- fetch_and_parse_stats(1)
    expect_type(result$yl, "integer")
    expect_type(result$dr, "integer")
    expect_type(result$toplam, "integer")
    expect_type(result$universite, "character")
  })
})

test_that("university stats preserves correct values after coercion", {
  with_fake_stats_html(mock_stats_html(1), {
    result <- fetch_and_parse_stats(1)
    expect_identical(result$yl, c(100L, 200L))
    expect_identical(result$dr, c(50L, 80L))
    expect_identical(result$toplam, c(160L, 300L))
  })
})

test_that("year stats coerces year column to integer", {
  with_fake_stats_html(mock_stats_html(2), {
    result <- fetch_and_parse_stats(2)
    expect_type(result$yil, "integer")
    expect_identical(result$yil, c(2020L, 2021L))
  })
})

test_that("type stats (islem=4) coerces all columns to numeric", {
  with_fake_stats_html(mock_stats_html(4), {
    result <- fetch_and_parse_stats(4)
    expect_type(result$tur, "double")
    expect_type(result$toplam, "double")
    expect_identical(result$toplam, c(5000, 3000))
  })
})

test_that("subject stats (islem=3) coerces from column 4 onward", {
  with_fake_stats_html(mock_stats_html(3), {
    result <- fetch_and_parse_stats(3)
    expect_type(result$konu, "character")
    expect_type(result$yl, "integer")
    expect_type(result$dr, "integer")
  })
})

# ---- Turkish character normalization in headers ----

test_that("headers with Turkish characters are normalized to ASCII", {
  turkish_headers <- rvest::read_html(
    "
    <html><body>
    <table><tr><td>ignore</td></tr></table>
    <table>
      <tr><td colspan='3'>Title</td></tr>
      <tr>
        <td>\u00DC niversite</td>
        <td>\u015E ehir</td>
        <td>\u00D6 \u011F renci</td>
      </tr>
      <tr><td>A</td><td>20</td><td>10</td></tr>
    </table>
    </body></html>
  "
  )
  with_fake_stats_html(turkish_headers, {
    result <- fetch_and_parse_stats(1)
    col_names <- names(result)
    expect_false(any(grepl(
      "[\u00FC\u015F\u00F6\u011F\u0131\u00E7]",
      col_names
    )))
  })
})

# ---- Whitespace and empty cell handling ----

test_that("cells with only whitespace become NA", {
  whitespace_cells <- rvest::read_html(
    "
    <html><body>
    <table><tr><td>ignore</td></tr></table>
    <table>
      <tr><td colspan='3'>Title</td></tr>
      <tr><td>Name</td><td>Count1</td><td>Count2</td></tr>
      <tr><td>Valid</td><td>10</td><td>   </td></tr>
    </table>
    </body></html>
  "
  )
  with_fake_stats_html(whitespace_cells, {
    result <- fetch_and_parse_stats(1)
    expect_identical(nrow(result), 1L)
    expect_true(is.na(result$count2))
  })
})

test_that("rows with all empty cells are filtered out", {
  mixed_rows <- rvest::read_html(
    "
    <html><body>
    <table><tr><td>ignore</td></tr></table>
    <table>
      <tr><td colspan='2'>Title</td></tr>
      <tr><td>Name</td><td>Val</td></tr>
      <tr><td>Real</td><td>42</td></tr>
      <tr><td>  </td><td>  </td></tr>
      <tr><td>Also Real</td><td>99</td></tr>
    </table>
    </body></html>
  "
  )
  with_fake_stats_html(mixed_rows, {
    result <- fetch_and_parse_stats(1)
    expect_identical(nrow(result), 2L)
    expect_identical(result$name, c("Real", "Also Real"))
  })
})

# ---- Exported wrappers ----

test_that("stats_universities renames universite to university", {
  with_fake_stats_html(mock_stats_html(1), {
    result <- stats_universities()
    expect_s3_class(result, "tbl_df")
    expect_true("university" %in% names(result))
    expect_false("universite" %in% names(result))
    expect_identical(nrow(result), 2L)
  })
})

test_that("stats_years renames yil to year", {
  with_fake_stats_html(mock_stats_html(2), {
    result <- stats_years()
    expect_s3_class(result, "tbl_df")
    expect_true("year" %in% names(result))
    expect_false("yil" %in% names(result))
    expect_identical(result$year, c(2020L, 2021L))
  })
})

test_that("stats_subjects drops columns 2-3 and renames konu to subject", {
  with_fake_stats_html(mock_stats_html(3), {
    result <- stats_subjects()
    expect_s3_class(result, "tbl_df")
    expect_true("subject" %in% names(result))
    expect_false("konu" %in% names(result))
    expect_false("x2" %in% names(result))
    expect_false("x3" %in% names(result))
    expect_identical(result$subject, c("Fizik", "Kimya"))
  })
})

test_that("stats_types returns all-numeric tibble", {
  with_fake_stats_html(mock_stats_html(4), {
    result <- stats_types()
    expect_s3_class(result, "tbl_df")
    expect_true(all(vapply(result, is.numeric, logical(1))))
    expect_identical(nrow(result), 2L)
  })
})

# ---- Malformed HTML edge cases ----

test_that("extra nested tags inside table cells are handled", {
  nested_tags <- rvest::read_html(
    "
    <html><body>
    <table><tr><td>ignore</td></tr></table>
    <table>
      <tr><td colspan='3'>Title</td></tr>
      <tr><td>Name</td><td>A</td><td>B</td></tr>
      <tr><td><b>Bold Uni</b></td><td><span>55</span></td><td>33</td></tr>
    </table>
    </body></html>
  "
  )
  with_fake_stats_html(nested_tags, {
    result <- fetch_and_parse_stats(1)
    expect_identical(nrow(result), 1L)
    expect_identical(result$name, "Bold Uni")
    expect_identical(result$a, 55L)
  })
})

test_that("table with many empty filler rows returns only real data", {
  filler_rows <- rvest::read_html(
    "
    <html><body>
    <table><tr><td>ignore</td></tr></table>
    <table>
      <tr><td colspan='2'>Title</td></tr>
      <tr><td>Key</td><td>Val</td></tr>
      <tr><td></td><td></td></tr>
      <tr><td>  </td><td>  </td></tr>
      <tr><td>Actual</td><td>7</td></tr>
      <tr><td></td><td></td></tr>
    </table>
    </body></html>
  "
  )
  with_fake_stats_html(filler_rows, {
    result <- fetch_and_parse_stats(1)
    expect_identical(nrow(result), 1L)
    expect_identical(result$key, "Actual")
  })
})

test_that("single data row table works correctly", {
  single_row <- rvest::read_html(
    "
    <html><body>
    <table><tr><td>ignore</td></tr></table>
    <table>
      <tr><td colspan='2'>Title</td></tr>
      <tr><td>Name</td><td>Count</td></tr>
      <tr><td>Only</td><td>1</td></tr>
    </table>
    </body></html>
  "
  )
  with_fake_stats_html(single_row, {
    result <- fetch_and_parse_stats(1)
    expect_identical(nrow(result), 1L)
    expect_identical(result$name, "Only")
    expect_identical(result$count, 1L)
  })
})
