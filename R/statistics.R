#' Fetch a statistics page by operation code and parse the HTML table
#' @noRd
fetch_and_parse_stats <- function(islem) {
  cli::cli_alert_info("Fetching statistics...")

  resp <- create_session() |>
    httr2::req_url(paste0(base_url, "IstatistikiBilgiler")) |>
    httr2::req_url_query(islem = islem) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) != 200) {
    cli::cli_abort(
      "Failed to fetch statistics (status {httr2::resp_status(resp)})"
    )
  }

  html <- httr2::resp_body_html(resp)
  tables <- rvest::html_table(html)

  if (length(tables) < 2) {
    cli::cli_abort("Could not find statistics table in response")
  }

  stats_table <- tables[[2]]

  if (nrow(stats_table) < 3) {
    return(tibble::tibble())
  }

  headers <- as.character(stats_table[2, ]) |>
    clean_text() |>
    stringr::str_replace_all("\\s+", "_") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("\u0131", "i") |>
    stringr::str_replace_all("\u00F6", "o") |>
    stringr::str_replace_all("\u00FC", "u") |>
    stringr::str_replace_all("\u015F", "s") |>
    stringr::str_replace_all("\u00E7", "c") |>
    stringr::str_replace_all("\u011F", "g")

  stats_rows <- stats_table[3:nrow(stats_table), ]
  colnames(stats_rows) <- headers

  stats_rows <- stats_rows |>
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

  if (islem == 4) {
    stats_rows <- stats_rows |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
  } else {
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
  }

  return(tibble::as_tibble(stats_rows))
}

#' Get thesis statistics by university
#'
#' @return A tibble containing thesis counts per university and type.
#' @export
#'
#' @examples
#' \dontrun{
#' stats <- stats_universities()
#' }
stats_universities <- function() {
  return(
    fetch_and_parse_stats(1) |>
      dplyr::rename(university = "universite")
  )
}

#' Get thesis statistics by year
#'
#' @return A tibble containing thesis counts per year and type.
#' @export
#'
#' @examples
#' \dontrun{
#' stats <- stats_years()
#' }
stats_years <- function() {
  return(
    fetch_and_parse_stats(2) |>
      dplyr::rename(year = "yil")
  )
}

#' Get thesis statistics by subject
#'
#' @return A tibble containing thesis counts per subject and type.
#' @export
#'
#' @examples
#' \dontrun{
#' stats <- stats_subjects()
#' }
stats_subjects <- function() {
  return(
    fetch_and_parse_stats(3) |>
      dplyr::select(-2, -3) |>
      dplyr::rename(subject = "konu")
  )
}

#' Get thesis statistics by type
#'
#' @return A tibble containing total thesis counts per type.
#' @export
#'
#' @examples
#' \dontrun{
#' stats <- stats_types()
#' }
stats_types <- function() {
  return(fetch_and_parse_stats(4))
}
