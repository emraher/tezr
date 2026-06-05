#' Get detailed information about a thesis
#'
#' Retrieves the full details of one or more theses from the Turkish National
#' Thesis Center using encoded IDs from search results.
#'
#' @name detail
#' @param detail_id Character vector. Encoded thesis detail identifier(s) from
#'   the `detail_id` column of search results. Treat these values as opaque.
#'   They may contain one or more portal identifiers depending on the current
#'   National Thesis Center markup.
#' @param progress Logical. Show text progress updates when fetching multiple
#'   theses? Default is TRUE.
#' @param ... Reserved for internal use.
#'
#' @return A tibble with thesis details (one row per thesis).
#'   Columns (in order):
#'   \itemize{
#'     \item thesis_no - Thesis number
#'     \item title_original - Original title
#'     \item title_translation - Title translation when available
#'     \item author - Author name
#'     \item advisor - Advisor name and title
#'     \item co_advisor - Co-advisor name(s). Multiple names are
#'       semicolon-separated, and absent values are `NA`.
#'     \item university - University name
#'     \item institute - Institute name
#'     \item division - Division name
#'     \item year - Year
#'     \item pages - Number of pages
#'     \item thesis_type_tr - Thesis type in Turkish, such as "Doktora" or
#'       "Yüksek Lisans"
#'     \item thesis_type_en - Thesis type in English, such as "Doctorate" or
#'       "Master's"
#'     \item language_tr - Language in Turkish, such as "Türkçe" or
#'       "İngilizce"
#'     \item language_en - Language in English (e.g., "Turkish", "English")
#'     \item subject_tr - Turkish subject classifications, semicolon-separated
#'       when multiple
#'     \item subject_en - English subject classifications, semicolon-separated
#'       when multiple
#'     \item abstract_original - Abstract in the thesis's original language when
#'       available
#'     \item abstract_translation - Translated abstract when available
#'     \item keywords_tr - Turkish keywords (includes keywords from index field)
#'     \item keywords_en - English keywords (includes keywords from index field)
#'     \item access_status - Access status (open/restricted)
#'     \item pdf_url - URL to download PDF (if available)
#'     \item detail_url - URL to the thesis detail page
#'   }
#'
#' @family detail functions
#' @export
#'
#' @examplesIf interactive()
#' # Search for theses
#' results <- search_basic("panel veri")
#'
#' # Get details for a single thesis
#' thesis_details <- detail(results$detail_id[1])
#'
#' # Get details for multiple theses (batch)
#' all_details <- detail(detail_id = results$detail_id)
#'
#' # Without progress updates
#' all_details <- detail(detail_id = results$detail_id, progress = FALSE)
detail <- function(
  detail_id,
  progress = TRUE,
  ...
) {
  if (missing(detail_id) || is.null(detail_id)) {
    cli::cli_abort("{.arg detail_id} is required")
  }

  if (!is.character(detail_id)) {
    cli::cli_abort("{.arg detail_id} must be a character vector")
  }

  if (length(detail_id) == 0) {
    cli::cli_abort("{.arg detail_id} must be a non-empty character vector")
  }

  thesis_count <- length(detail_id)

  if (thesis_count > 1) {
    return(fetch_batch_details(detail_id, progress = progress))
  }

  # Single thesis - fetch and return as tibble
  thesis_details <- fetch_single_thesis(detail_id)
  return(tibble::as_tibble(thesis_details))
}

#' Fetch details for multiple theses in parallel
#' @noRd
fetch_batch_details <- function(detail_id, progress = TRUE) {
  thesis_count <- length(detail_id)
  tezr_inform("Fetching details for {.val {thesis_count}} theses...")

  init_cache()

  detail_ids <- prepare_batch_detail_ids(detail_id)
  split <- split_cached_uncached(detail_ids$valid_ids)

  if (split$cached_count > 0) {
    tezr_success(
      "Found {.val {split$cached_count}} thesis detail{?s} in cache"
    )
  }

  if (length(split$uncached_ids) > 0) {
    split$results <- fetch_uncached_parallel(
      split$uncached_ids,
      split$results,
      progress = progress
    )
  }

  batch_result <- build_batch_detail_result(
    detail_id,
    split$results,
    detail_ids$na_count
  )

  finalize_batch_detail_result(batch_result, thesis_count)
}

#' Return valid detail IDs and warn about missing inputs
#' @noRd
prepare_batch_detail_ids <- function(detail_id) {
  valid_mask <- !is.na(detail_id)
  na_count <- sum(!valid_mask)

  if (na_count > 0) {
    cli::cli_alert_warning("Skipping {.val {na_count}} NA detail_id value{?s}")
  }

  list(
    valid_ids = detail_id[valid_mask],
    na_count = na_count
  )
}

#' Finalize a batch detail fetch result
#' @noRd
finalize_batch_detail_result <- function(batch_result, thesis_count) {
  if (length(batch_result$successful) == 0) {
    cli::cli_alert_warning("All {.val {thesis_count}} fetches failed")
    return(tibble::tibble())
  }

  details_df <- dplyr::bind_rows(batch_result$successful)

  if (batch_result$failed_count > 0) {
    cli::cli_alert_warning(
      paste0(
        "{.val {batch_result$failed_count}} of ",
        "{.val {thesis_count}} fetches failed"
      )
    )
  }

  tezr_success(
    "Retrieved details for {.val {nrow(details_df)}} theses"
  )
  details_df
}

#' Assemble ordered batch details and count failed fetches
#' @noRd
build_batch_detail_result <- function(detail_id, results, na_count) {
  thesis_count <- length(detail_id)

  details_list <- lapply(detail_id, function(tid) {
    if (is.na(tid)) {
      return(NULL)
    }
    results[[tid]]
  })

  successful <- Filter(Negate(is.null), details_list)
  failed_count <- thesis_count - na_count - length(successful)

  list(
    successful = successful,
    failed_count = failed_count
  )
}

#' Split IDs into cached and uncached groups
#' @return List with `results` (named list), `uncached_ids`, and `cached_count`
#' @noRd
split_cached_uncached <- function(valid_ids) {
  results <- stats::setNames(vector("list", length(valid_ids)), valid_ids)
  uncached_ids <- character()

  for (tid in valid_ids) {
    cache_key <- make_detail_key(tid, "")
    cached_value <- get_cached(
      tezr_env$detail_cache,
      cache_key,
      tezr_env$detail_ttl
    )
    if (!is.null(cached_value)) {
      results[[tid]] <- cached_value
    } else {
      uncached_ids <- c(uncached_ids, tid)
    }
  }

  cached_count <- length(valid_ids) - length(uncached_ids)
  return(list(
    results = results,
    uncached_ids = uncached_ids,
    cached_count = cached_count
  ))
}

#' Fetch uncached IDs in parallel and merge into results
#' @noRd
fetch_uncached_parallel <- function(uncached_ids, results, progress = FALSE) {
  if (!has_session()) {
    tezr_inform("Initializing session...")
    init_session()
  }

  refresh_session_if_needed()

  total_uncached <- length(uncached_ids)
  if (total_uncached == 0L) {
    return(results)
  }

  chunk_size <- if (isTRUE(progress)) 10L else total_uncached

  completed <- 0L
  chunk_starts <- seq.int(1L, total_uncached, by = chunk_size)

  for (chunk_start in chunk_starts) {
    chunk_end <- min(chunk_start + chunk_size - 1L, total_uncached)
    chunk_ids <- uncached_ids[chunk_start:chunk_end]

    reqs <- lapply(chunk_ids, build_detail_request)
    resps <- perform_parallel(reqs)

    for (idx in seq_along(chunk_ids)) {
      tid <- chunk_ids[[idx]]
      parsed <- parse_detail_response(resps[[idx]], tid)
      if (!is.null(parsed)) {
        results[[tid]] <- parsed
        increment_request_count()
      }
    }

    completed <- chunk_end
    if (isTRUE(progress)) {
      tezr_inform(paste0(
        "Fetched {.val {completed}}/{.val {total_uncached}} ",
        "uncached detail record{?s}"
      ))
    }
  }

  return(results)
}

#' Internal function to fetch a single thesis
#' @noRd
fetch_single_thesis <- function(detail_id) {
  init_cache()
  cache_key <- make_detail_key(detail_id, "")

  cached_details <- get_cached(
    tezr_env$detail_cache,
    cache_key,
    tezr_env$detail_ttl
  )

  if (!is.null(cached_details)) {
    tezr_success("Returning cached thesis details")
    return(cached_details)
  }

  if (!has_session()) {
    tezr_inform("Initializing session...")
    init_session()
  }

  tezr_inform("Fetching thesis details...")

  refresh_session_if_needed()
  increment_request_count()

  req <- build_detail_request(detail_id)
  resp <- httr2::req_perform(req)

  details <- parse_and_cache_detail_response(
    resp,
    detail_id,
    on_http_error = "abort"
  )

  tezr_success("Retrieved details for thesis")
  return(details)
}

#' Build an httr2 request for a thesis detail page
#' @noRd
build_detail_request <- function(detail_id) {
  detail_parts <- split_detail_id(detail_id)

  req <- create_session(apply_rate_limit = detail_rate_limit_enabled()) |>
    httr2::req_url(paste0(base_url, endpoints$detail)) |>
    httr2::req_url_query(id = detail_parts$id) |>
    httr2::req_retry(max_tries = 3, backoff = ~2) |>
    httr2::req_error(is_error = function(resp) FALSE)

  if (!is.na(detail_parts$no)) {
    req <- httr2::req_url_query(req, no = detail_parts$no)
  }

  req
}

#' Should detail requests apply rate limiting?
#' @noRd
detail_rate_limit_enabled <- function() {
  return(isTRUE(getOption("tezr.detail_rate_limit", TRUE)))
}

#' Parse and cache one thesis detail response
#' @noRd
parse_and_cache_detail_response <- function(
  resp,
  detail_id,
  on_http_error = c("warn", "abort")
) {
  on_http_error <- match.arg(on_http_error)
  status <- httr2::resp_status(resp)

  if (status != 200) {
    if (identical(on_http_error, "abort")) {
      cli::cli_abort(
        c(
          paste0(
            "Failed to fetch thesis details with status {status} ",
            "for id {.val {detail_id}}."
          ),
          "i" = paste0(
            "The NTC portal may be unavailable, may have changed its detail ",
            "page markup, or may have rejected the request."
          )
        )
      )
    }

    cli::cli_alert_warning("HTTP {status} for detail_id {.val {detail_id}}")
    return(NULL)
  }

  html <- httr2::resp_body_html(resp)
  details <- parse_detail_page(html)
  detail_parts <- split_detail_id(detail_id)
  details$detail_url <- paste0(
    base_url,
    endpoints$detail,
    "?id=",
    utils::URLencode(detail_parts$id, reserved = TRUE),
    if (!is.na(detail_parts$no)) {
      paste0("&no=", utils::URLencode(detail_parts$no, reserved = TRUE))
    } else {
      ""
    }
  )

  cache_key <- make_detail_key(detail_id, "")
  set_cached(tezr_env$detail_cache, cache_key, details)

  details
}

#' Parse a detail page response and cache the result
#' @return Parsed detail list, or NULL on failure
#' @noRd
parse_detail_response <- function(resp, detail_id) {
  tryCatch(
    {
      parse_and_cache_detail_response(resp, detail_id, on_http_error = "warn")
    },
    error = function(e) {
      cli::cli_alert_warning(
        "Failed to parse details for {.val {detail_id}}: {e$message}"
      )
      return(NULL)
    }
  )
}

#' Thin wrapper around httr2::req_perform_parallel for testability
#' @noRd
perform_parallel <- function(reqs) {
  return(httr2::req_perform_parallel(
    reqs,
    on_error = "continue",
    max_active = 5L,
    progress = FALSE
  ))
}
