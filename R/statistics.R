#' Fetch a statistics page by operation code and parse the HTML table
#' @noRd
fetch_and_parse_stats <- function(islem) {
  tezr_inform("Fetching statistics...")

  stats_table <- fetch_stats_table(islem)

  if (nrow(stats_table) < 3) {
    return(tibble::tibble())
  }

  stats_rows <- normalize_stats_rows(stats_table)
  stats_rows <- coerce_stats_rows(stats_rows, islem)

  return(tibble::as_tibble(stats_rows))
}

#' Fetch the raw statistics table for an operation code
#' @noRd
fetch_stats_table <- function(islem) {
  resp <- create_session() |>
    httr2::req_url(paste0(base_url, "IstatistikiBilgiler")) |>
    httr2::req_url_query(islem = islem) |>
    httr2::req_perform()

  abort_on_stats_fetch_error(resp)
  tables <- rvest::html_table(httr2::resp_body_html(resp))

  if (length(tables) < 2) {
    cli::cli_abort("Could not find statistics table in response")
  }

  tables[[2]]
}

#' Abort when a statistics response is not successful
#' @noRd
abort_on_stats_fetch_error <- function(resp) {
  if (httr2::resp_status(resp) == 200) {
    return(invisible(NULL))
  }

  status <- httr2::resp_status(resp)
  cli::cli_abort(
    c(
      "Failed to fetch statistics with status {status}.",
      "i" = paste0(
        "The NTC portal may be unavailable or may have changed the ",
        "statistics endpoint."
      )
    )
  )
}

#' Normalize statistics table headers and remove empty rows
#' @noRd
normalize_stats_rows <- function(stats_table) {
  headers <- normalize_stats_headers(stats_table)
  stats_rows <- stats_table[3:nrow(stats_table), ]
  colnames(stats_rows) <- headers

  clean_stats_rows(stats_rows)
}

#' Normalize statistics table headers
#' @noRd
normalize_stats_headers <- function(stats_table) {
  as.character(stats_table[2, ]) |>
    clean_text() |>
    stringr::str_replace_all("\\s+", "_") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(stringr::fixed("\u0131"), "i") |>
    stringr::str_replace_all(stringr::fixed("\u00F6"), "o") |>
    stringr::str_replace_all(stringr::fixed("\u00FC"), "u") |>
    stringr::str_replace_all(stringr::fixed("\u015F"), "s") |>
    stringr::str_replace_all(stringr::fixed("\u00E7"), "c") |>
    stringr::str_replace_all(stringr::fixed("\u011F"), "g")
}

#' Clean statistics table cells and drop all-empty rows
#' @noRd
clean_stats_rows <- function(stats_rows) {
  stats_rows |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ {
        val <- clean_text(.)
        val[val == ""] <- NA
        val
      }
    )) |>
    dplyr::filter(
      if (ncol(stats_rows) > 0) {
        !dplyr::if_all(dplyr::everything(), is.na)
      } else {
        TRUE
      }
    )
}

#' Coerce statistics table columns to their expected numeric types
#' @noRd
coerce_stats_rows <- function(stats_rows, islem) {
  if (islem == 4) {
    stats_rows <- stats_rows |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
    return(stats_rows)
  }

  start_col <- if (islem == 3) 4 else 2
  if (ncol(stats_rows) >= start_col) {
    stats_rows <- stats_rows |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(start_col:ncol(stats_rows)),
        as.integer
      ))
  }

  if (islem == 2) {
    stats_rows <- stats_rows |> dplyr::mutate(yil = as.integer(.data$yil))
  }

  stats_rows
}

#' Get thesis statistics by university
#'
#' @return A tibble containing thesis counts per university and type.
#' @family statistics functions
#' @export
#'
#' @examplesIf interactive()
#' stats <- stats_universities()
stats_universities <- function() {
  stats <- fetch_and_parse_stats(1) |>
    dplyr::rename(university = "universite")

  return(stats)
}

#' Get thesis statistics by year
#'
#' @return A tibble containing thesis counts per year and type.
#' @family statistics functions
#' @export
#'
#' @examplesIf interactive()
#' stats <- stats_years()
stats_years <- function() {
  stats <- fetch_and_parse_stats(2) |>
    dplyr::rename(year = "yil")

  return(stats)
}

#' Get thesis statistics by subject
#'
#' @return A tibble containing thesis counts per subject and type.
#' @family statistics functions
#' @export
#'
#' @examplesIf interactive()
#' stats <- stats_subjects()
stats_subjects <- function() {
  stats <- fetch_and_parse_stats(3) |>
    dplyr::select(-2, -3) |>
    dplyr::rename(subject = "konu")

  return(stats)
}

#' Get thesis statistics by type
#'
#' @return A tibble containing total thesis counts per type.
#' @family statistics functions
#' @export
#'
#' @examplesIf interactive()
#' stats <- stats_types()
stats_types <- function() {
  return(fetch_and_parse_stats(4))
}
