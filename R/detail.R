#' Get detailed information about a thesis
#'
#' Retrieves the full details of one or more theses from the Turkish National
#' Thesis Center using encoded IDs from search results.
#'
#' @name detail
#' @param detail_id Character vector, detail URL, or search-result data frame.
#'   Character values may be encoded IDs from the `detail_id` column or
#'   redesigned YOK detail URLs. A data frame returned by a search function can
#'   be passed directly. Accepts multiple values or rows for batch retrieval.
#' @param progress Logical. Show text progress updates when fetching multiple
#'   theses? Default is TRUE.
#' @param encrypted_no Character vector. Optional encrypted thesis number from
#'   the `encrypted_no` column of redesigned search results. When available,
#'   `detail()` includes it in the YOK detail request and uses the JSON detail
#'   endpoint to add citation metadata.
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
#'     \item co_advisor - Co-advisor name(s) (semicolon-separated if multiple; NA when absent)
#'     \item university - University name
#'     \item institute - Institute name
#'     \item division - Division name
#'     \item year - Year
#'     \item pages - Number of pages
#'     \item thesis_type_tr - Thesis type in Turkish (e.g., "Doktora", "Yüksek Lisans")
#'     \item thesis_type_en - Thesis type in English (e.g., "Doctorate", "Master's")
#'     \item language_tr - Language in Turkish (e.g., "Türkçe", "İngilizce")
#'     \item language_en - Language in English (e.g., "Turkish", "English")
#'     \item subject_tr - Turkish subject classifications (semicolon-separated when multiple)
#'     \item subject_en - English subject classifications (semicolon-separated when multiple)
#'     \item abstract_original - Abstract in the thesis's original language when available
#'     \item abstract_translation - Translated abstract when available
#'     \item keywords_tr - Turkish keywords (includes keywords from index field)
#'     \item keywords_en - English keywords (includes keywords from index field)
#'     \item access_status - Access status (open/restricted)
#'     \item pdf_url - URL to download PDF (if available)
#'     \item detail_url - URL to the thesis detail page
#'     \item citation_apa, citation_ieee, citation_mla, citation_chicago,
#'       citation_harvard - Citation strings when YOK's JSON detail endpoint is
#'       available
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Search for theses
#' results <- search_basic("panel veri")
#'
#' # Get details for a single thesis, including citation metadata when present
#' thesis_details <- detail(results[1, ])
#'
#' # Get details for multiple theses (batch)
#' all_details <- detail(results)
#'
#' # Without progress updates
#' all_details <- detail(results, progress = FALSE)
#' }
detail <- function(
  detail_id,
  progress = TRUE,
  encrypted_no = NULL,
  ...
) {
  if (missing(detail_id) || is.null(detail_id)) {
    cli::cli_abort("{.arg detail_id} is required")
  }
  progress <- validate_detail_progress(progress)

  detail_input <- normalize_detail_input(detail_id, encrypted_no)
  detail_id <- detail_input$detail_id
  encrypted_no <- detail_input$encrypted_no

  normalized_refs <- normalize_detail_refs(detail_id, encrypted_no)
  detail_id <- normalized_refs$detail_id
  encrypted_no <- normalized_refs$encrypted_no

  thesis_count <- length(detail_id)

  if (thesis_count > 1) {
    return(fetch_batch_details(
      detail_id,
      progress = progress,
      encrypted_no = encrypted_no
    ))
  }

  # Single thesis - fetch and return as tibble
  thesis_details <- if (is.na(encrypted_no)) {
    fetch_single_thesis(detail_id)
  } else {
    fetch_single_thesis(detail_id, encrypted_no = encrypted_no)
  }
  return(tibble::as_tibble(thesis_details))
}

#' Validate detail progress flag
#' @noRd
validate_detail_progress <- function(progress) {
  if (
    !is.logical(progress) ||
      length(progress) != 1L ||
      is.na(progress)
  ) {
    cli::cli_abort("{.arg progress} must be TRUE or FALSE")
  }

  progress
}

#' Normalize detail input before URL/id parsing
#' @noRd
normalize_detail_input <- function(detail_id, encrypted_no = NULL) {
  if (is.data.frame(detail_id)) {
    return(normalize_detail_frame(detail_id, encrypted_no))
  }

  if (!is.character(detail_id)) {
    cli::cli_abort(
      paste0(
        "{.arg detail_id} must be a character vector, detail URL, or ",
        "search-result data frame"
      )
    )
  }

  if (length(detail_id) == 0) {
    cli::cli_abort("{.arg detail_id} must be non-empty")
  }

  list(
    detail_id = detail_id,
    encrypted_no = encrypted_no
  )
}

#' Extract detail IDs and encrypted numbers from search-result rows
#' @noRd
normalize_detail_frame <- function(search_results, encrypted_no = NULL) {
  if (nrow(search_results) == 0) {
    cli::cli_abort("{.arg detail_id} must contain at least one row")
  }

  has_detail_id <- "detail_id" %in% names(search_results)
  has_detail_url <- "detail_url" %in% names(search_results)

  if (!has_detail_id && !has_detail_url) {
    cli::cli_abort(
      "{.arg detail_id} data frames must contain {.col detail_id} or {.col detail_url}"
    )
  }

  if (!has_detail_id) {
    detail_id <- as.character(search_results$detail_url)
  } else if (is.null(encrypted_no) && has_detail_url) {
    detail_id <- as.character(search_results$detail_url)
    fallback_ids <- as.character(search_results$detail_id)
    has_url <- !is.na(detail_id) & nzchar(detail_id)
    detail_id[!has_url] <- fallback_ids[!has_url]
  } else {
    detail_id <- as.character(search_results$detail_id)
  }

  if (is.null(encrypted_no) && "encrypted_no" %in% names(search_results)) {
    encrypted_no <- as.character(search_results$encrypted_no)
  }

  list(
    detail_id = detail_id,
    encrypted_no = encrypted_no
  )
}

#' Normalize detail identifiers and detail URLs
#' @noRd
normalize_detail_refs <- function(detail_id, encrypted_no = NULL) {
  if (!is.null(encrypted_no)) {
    if (!is.character(encrypted_no)) {
      cli::cli_abort("{.arg encrypted_no} must be a character vector")
    }

    if (!length(encrypted_no) %in% c(1L, length(detail_id))) {
      cli::cli_abort(
        "{.arg encrypted_no} must have length 1 or match {.arg detail_id}"
      )
    }

    encrypted_no <- rep(encrypted_no, length.out = length(detail_id))
  } else {
    encrypted_no <- rep(NA_character_, length(detail_id))
  }

  normalized_ids <- detail_id

  for (ref_index in seq_along(detail_id)) {
    parsed_ref <- parse_detail_ref(detail_id[[ref_index]])
    if (!is.null(parsed_ref$detail_id)) {
      normalized_ids[[ref_index]] <- parsed_ref$detail_id
    }
    if (is.na(encrypted_no[[ref_index]]) && !is.null(parsed_ref$encrypted_no)) {
      encrypted_no[[ref_index]] <- parsed_ref$encrypted_no
    }
  }

  blank_refs <- is.na(normalized_ids) | !nzchar(trimws(normalized_ids))
  normalized_ids[blank_refs] <- NA_character_
  if (all(blank_refs)) {
    cli::cli_abort("{.arg detail_id} must contain at least one non-empty value")
  }

  list(
    detail_id = normalized_ids,
    encrypted_no = encrypted_no
  )
}

#' Parse id/no parameters from a YOK detail URL
#' @noRd
parse_detail_ref <- function(detail_ref) {
  if (
    is.na(detail_ref) ||
      !grepl("tezDetay\\.jsp", detail_ref, fixed = FALSE) ||
      !grepl("?", detail_ref, fixed = TRUE)
  ) {
    return(list(detail_id = NULL, encrypted_no = NULL))
  }

  query_string <- sub("^[^?]*\\?", "", detail_ref)
  query_string <- sub("#.*$", "", query_string)
  query_parts <- strsplit(query_string, "&", fixed = TRUE)[[1]]
  query_values <- list()

  for (query_part in query_parts) {
    key_value <- strsplit(query_part, "=", fixed = TRUE)[[1]]
    if (length(key_value) < 2L) {
      next
    }

    query_key <- utils::URLdecode(key_value[[1]])
    query_value <- utils::URLdecode(paste(key_value[-1], collapse = "="))
    query_values[[query_key]] <- query_value
  }

  list(
    detail_id = query_values[["id"]],
    encrypted_no = query_values[["no"]]
  )
}

#' Fetch details for multiple theses in parallel
#' @noRd
fetch_batch_details <- function(
  detail_id,
  progress = TRUE,
  encrypted_no = NULL
) {
  thesis_count <- length(detail_id)
  cli::cli_alert_info("Fetching details for {.val {thesis_count}} theses...")

  init_cache()
  if (is.null(encrypted_no)) {
    encrypted_no <- rep(NA_character_, length(detail_id))
  }

  valid_mask <- !is.na(detail_id)
  na_count <- sum(!valid_mask)
  if (na_count > 0) {
    cli::cli_alert_warning("Skipping {.val {na_count}} NA detail_id value{?s}")
  }
  valid_ids <- detail_id[valid_mask]
  valid_encrypted_no <- encrypted_no[valid_mask]

  split <- split_cached_uncached(valid_ids, valid_encrypted_no)

  if (split$cached_count > 0) {
    cli::cli_alert_success(
      "Found {.val {split$cached_count}} thesis detail{?s} in cache"
    )
  }

  if (length(split$uncached_ids) > 0) {
    split$results <- fetch_uncached_parallel(
      split$uncached_ids,
      split$results,
      encrypted_no = split$uncached_encrypted_no,
      progress = progress
    )
  }

  details_list <- lapply(detail_id, function(tid) {
    if (is.na(tid)) {
      return(NULL)
    }
    split$results[[tid]]
  })

  successful <- Filter(Negate(is.null), details_list)
  failed_count <- thesis_count - na_count - length(successful)

  if (length(successful) == 0) {
    cli::cli_alert_warning("All {.val {thesis_count}} fetches failed")
    return(tibble::tibble())
  }

  details_df <- dplyr::bind_rows(successful)

  if (failed_count > 0) {
    cli::cli_alert_warning(
      "{.val {failed_count}} of {.val {thesis_count}} fetches failed"
    )
  }

  cli::cli_alert_success(
    "Retrieved details for {.val {nrow(details_df)}} theses"
  )
  return(details_df)
}

#' Split IDs into cached and uncached groups
#' @return List with `results` (named list), `uncached_ids`, and `cached_count`
#' @noRd
split_cached_uncached <- function(valid_ids, encrypted_no = NULL) {
  if (is.null(encrypted_no)) {
    encrypted_no <- rep(NA_character_, length(valid_ids))
  }

  split_cached_uncached_with_encrypted(valid_ids, encrypted_no)
}

#' Split IDs into cached and uncached groups with encrypted thesis numbers
#' @noRd
split_cached_uncached_with_encrypted <- function(valid_ids, encrypted_no) {
  results <- stats::setNames(vector("list", length(valid_ids)), valid_ids)
  uncached_ids <- character()
  uncached_encrypted_no <- character()

  for (id_index in seq_along(valid_ids)) {
    tid <- valid_ids[[id_index]]
    thesis_no <- encrypted_no[[id_index]] %|na|% ""
    cache_key <- make_detail_key(tid, thesis_no)
    cached_value <- get_cached(
      tezr_env$detail_cache,
      cache_key,
      tezr_env$detail_ttl
    )
    if (!is.null(cached_value)) {
      results[[tid]] <- cached_value
    } else {
      uncached_ids <- c(uncached_ids, tid)
      uncached_encrypted_no <- c(
        uncached_encrypted_no,
        encrypted_no[[id_index]]
      )
    }
  }

  cached_count <- length(valid_ids) - length(uncached_ids)
  return(list(
    results = results,
    uncached_ids = uncached_ids,
    uncached_encrypted_no = uncached_encrypted_no,
    cached_count = cached_count
  ))
}

#' Fetch uncached IDs in parallel and merge into results
#' @noRd
fetch_uncached_parallel <- function(
  uncached_ids,
  results,
  encrypted_no = NULL,
  progress = FALSE
) {
  if (!has_session()) {
    cli::cli_alert_info("Initializing session...")
    init_session()
  }

  refresh_session_if_needed()

  total_uncached <- length(uncached_ids)
  if (total_uncached == 0L) {
    return(results)
  }

  if (is.null(encrypted_no)) {
    encrypted_no <- rep(NA_character_, total_uncached)
  }

  chunk_size <- if (isTRUE(progress)) 10L else total_uncached

  chunk_starts <- seq.int(1L, total_uncached, by = chunk_size)

  for (chunk_start in chunk_starts) {
    chunk_end <- min(chunk_start + chunk_size - 1L, total_uncached)
    chunk_ids <- uncached_ids[chunk_start:chunk_end]
    chunk_encrypted_no <- encrypted_no[chunk_start:chunk_end]

    reqs <- vector("list", length(chunk_ids))
    for (request_index in seq_along(chunk_ids)) {
      reqs[[request_index]] <- if (is.na(chunk_encrypted_no[[request_index]])) {
        build_detail_request(chunk_ids[[request_index]])
      } else {
        build_detail_request(
          chunk_ids[[request_index]],
          chunk_encrypted_no[[request_index]]
        )
      }
    }
    resps <- perform_parallel(reqs)

    for (idx in seq_along(chunk_ids)) {
      tid <- chunk_ids[[idx]]
      resp <- resps[[idx]]
      if (inherits(resp, "error")) {
        cli::cli_alert_warning(
          "Skipping detail record {.val {tid}} after request error: {conditionMessage(resp)}"
        )
        next
      }

      parsed <- if (is.na(chunk_encrypted_no[[idx]])) {
        parse_detail_response(resp, tid)
      } else {
        parse_detail_response(resp, tid, chunk_encrypted_no[[idx]])
      }
      if (!is.null(parsed)) {
        results[[tid]] <- parsed
        increment_request_count()
      }
    }

    if (isTRUE(progress)) {
      cli::cli_alert_info(
        "Fetched {.val {chunk_end}}/{.val {total_uncached}} uncached detail record{?s}"
      )
    }
  }

  return(results)
}

#' Internal function to fetch a single thesis
#' @noRd
fetch_single_thesis <- function(detail_id, encrypted_no = NULL) {
  init_cache()
  cache_key <- make_detail_key(detail_id, encrypted_no %|na|% "")

  cached_details <- get_cached(
    tezr_env$detail_cache,
    cache_key,
    tezr_env$detail_ttl
  )

  if (!is.null(cached_details)) {
    cli::cli_alert_success("Returning cached thesis details")
    return(cached_details)
  }

  if (!has_session()) {
    cli::cli_alert_info("Initializing session...")
    init_session()
  }

  cli::cli_alert_info("Fetching thesis details...")

  refresh_session_if_needed()
  increment_request_count()

  req <- create_session(apply_rate_limit = detail_rate_limit_enabled()) |>
    httr2::req_url(paste0(base_url, endpoints$detail)) |>
    httr2::req_url_query(
      id = detail_id,
      no = encrypted_no %|na|% NULL
    ) |>
    httr2::req_retry(max_tries = 3, backoff = ~2) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)

  details <- parse_and_cache_detail_response(
    resp,
    detail_id,
    encrypted_no = encrypted_no,
    on_http_error = "abort"
  )

  cli::cli_alert_success("Retrieved details for thesis")
  return(details)
}

#' Build an httr2 request for a thesis detail page
#' @noRd
build_detail_request <- function(detail_id, encrypted_no = NULL) {
  req <- create_session(apply_rate_limit = detail_rate_limit_enabled()) |>
    httr2::req_url(paste0(base_url, endpoints$detail)) |>
    httr2::req_url_query(
      id = detail_id,
      no = encrypted_no %|na|% NULL
    ) |>
    httr2::req_retry(max_tries = 3, backoff = ~2) |>
    httr2::req_error(is_error = function(resp) FALSE)

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
  encrypted_no = NULL,
  on_http_error = c("warn", "abort")
) {
  on_http_error <- match.arg(on_http_error)
  status <- httr2::resp_status(resp)

  if (status != 200) {
    if (identical(on_http_error, "abort")) {
      cli::cli_abort(
        "Failed to fetch thesis details (HTTP {status} for id {.val {detail_id}})"
      )
    }

    cli::cli_alert_warning("HTTP {status} for detail_id {.val {detail_id}}")
    return(NULL)
  }

  html <- httr2::resp_body_html(resp)
  details <- parse_detail_page(html)
  json_details <- fetch_detail_json_details(detail_id, encrypted_no)
  details <- merge_detail_json_details(details, json_details)
  details$detail_url <- build_detail_url(detail_id, encrypted_no)

  cache_key <- make_detail_key(detail_id, encrypted_no %|na|% "")
  set_cached(tezr_env$detail_cache, cache_key, details)

  details
}

#' Fetch rich JSON details when the encrypted thesis number is available
#' @noRd
fetch_detail_json_details <- function(detail_id, encrypted_no = NULL) {
  if (
    is.null(encrypted_no) || is.na(encrypted_no) || nchar(encrypted_no) == 0
  ) {
    return(NULL)
  }

  tryCatch(
    {
      resp <- create_session(apply_rate_limit = detail_rate_limit_enabled()) |>
        httr2::req_url(paste0(base_url, endpoints$detail_json)) |>
        httr2::req_url_query(kayitNo = detail_id, tezNo = encrypted_no) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform()

      if (httr2::resp_status(resp) != 200L) {
        return(NULL)
      }

      payload <- httr2::resp_body_string(resp) |>
        jsonlite::fromJSON(simplifyVector = FALSE)
      parse_detail_json_payload(payload)
    },
    error = function(error) {
      NULL
    }
  )
}

#' Merge optional JSON detail fields into parsed detail-page fields
#' @noRd
merge_detail_json_details <- function(details, json_details) {
  if (is.null(json_details)) {
    return(details)
  }

  for (field_name in names(json_details)) {
    if (
      !field_name %in% names(details) ||
        is.na(details[[field_name]]) ||
        (is.character(details[[field_name]]) &&
          nchar(details[[field_name]]) == 0)
    ) {
      details[[field_name]] <- json_details[[field_name]]
    }
  }

  details
}

#' Parse a detail page response and cache the result
#' @return Parsed detail list, or NULL on failure
#' @noRd
parse_detail_response <- function(resp, detail_id, encrypted_no = NULL) {
  tryCatch(
    {
      parse_and_cache_detail_response(
        resp,
        detail_id,
        encrypted_no = encrypted_no,
        on_http_error = "warn"
      )
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
  tryCatch(
    httr2::req_perform_parallel(
      reqs,
      on_error = "continue",
      max_active = 5L,
      progress = FALSE
    ),
    error = function(err) {
      cli::cli_alert_warning(
        "Parallel detail request failed. Retrying the batch sequentially."
      )
      lapply(reqs, function(req) {
        tryCatch(
          httr2::req_perform(req),
          error = function(sequential_error) sequential_error
        )
      })
    }
  )
}
