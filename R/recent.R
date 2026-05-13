#' List recent theses from YOK Tez
#'
#' Retrieves one of the recent-thesis lists exposed by YOK's TezIslemleri
#' endpoint. This is the only redesigned endpoint that lists theses without a
#' keyword.
#'
#' @param mode Character. One of `"last_15_days"` or `"current_year"`.
#' @param max_search_results Maximum rows to return from the server-visible
#'   batch. Default is 2000.
#' @param ignore_cache Logical. If `TRUE`, bypass cached recent-list results and
#'   fetch fresh data from the server.
#'
#' @return A tibble containing thesis records with the same columns as
#'   [search_basic()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' recent <- list_recent_theses()
#' this_year <- list_recent_theses(mode = "current_year")
#' }
list_recent_theses <- function(
  mode = c("last_15_days", "current_year"),
  max_search_results = 2000,
  ignore_cache = FALSE
) {
  mode <- rlang::arg_match(mode)
  validate_ignore_cache(ignore_cache)
  max_search_results <- validate_max_search_results(max_search_results)

  cache_key <- build_search_cache_key(
    type = "recent",
    params = list(mode = mode)
  )

  cached_results <- get_cached_search_result(
    cache_key,
    ignore_cache = ignore_cache
  )

  if (!is.null(cached_results)) {
    cached_count <- nrow(cached_results)
    if (nrow(cached_results) > max_search_results) {
      cached_results <- cached_results[seq_len(max_search_results), ]
      cli::cli_alert_success(
        sprintf(
          "Returning cached recent theses (%d of %d records)",
          nrow(cached_results),
          cached_count
        )
      )
      return(cached_results)
    }

    cli::cli_alert_success(
      sprintf("Returning cached recent theses (%d records)", cached_count)
    )
    return(cached_results)
  }

  search_data <- perform_recent_request(mode)
  recent_results <- annotate_search_results(
    search_data$search_results,
    total_count = search_data$total_count,
    paginated = FALSE
  )

  set_cached_search_result(
    cache_key,
    recent_results,
    ignore_cache = ignore_cache
  )

  if (nrow(recent_results) > max_search_results) {
    recent_results <- recent_results[seq_len(max_search_results), ]
  }

  recent_results
}

#' Perform one recent-list request
#' @noRd
perform_recent_request <- function(mode) {
  ensure_search_session()
  refresh_session_if_needed()
  increment_request_count()

  resp <- create_session() |>
    httr2::req_url_path_append(endpoints$recent) |>
    httr2::req_url_query(islem = recent_list_codes[[mode]]) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) != 200L) {
    cli::cli_abort(
      "Recent thesis request failed with status {httr2::resp_status(resp)}"
    )
  }

  html <- httr2::resp_body_html(resp)
  search_results <- parse_results_table(html)

  list(
    html = html,
    total_count = extract_total_count(html),
    search_results = search_results
  )
}
