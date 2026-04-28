# Shared request helpers for tezr package

#' Normalize cached basic-search payloads (legacy tibble or list payload)
#' @noRd
normalize_basic_cache_entry <- function(cache_entry) {
  if (is.null(cache_entry)) {
    return(NULL)
  }

  if (is.data.frame(cache_entry)) {
    return(list(
      results = cache_entry,
      total_count = nrow(cache_entry)
    ))
  }

  if (
    is.list(cache_entry) &&
      !is.null(cache_entry[["results"]]) &&
      !is.null(cache_entry[["total_count"]])
  ) {
    return(list(
      results = cache_entry[["results"]],
      total_count = as.integer(cache_entry[["total_count"]])
    ))
  }

  return(NULL)
}

#' Internal helper to execute search and parse results
#' @noRd
perform_search_request <- function(form_data) {
  refresh_session_if_needed()
  increment_request_count()

  session_request <- create_session()

  search_resp <- session_request |>
    httr2::req_url_path_append(endpoints$search) |>
    httr2::req_body_form(!!!form_data) |>
    httr2::req_perform()

  if (httr2::resp_status(search_resp) != 200) {
    cli::cli_abort(
      "Search request failed with status {httr2::resp_status(search_resp)}"
    )
  }

  search_results_resp <- session_request |>
    httr2::req_url_path_append(endpoints$results) |>
    httr2::req_perform()

  if (httr2::resp_status(search_results_resp) != 200) {
    cli::cli_abort("Failed to retrieve results page")
  }

  html <- httr2::resp_body_html(search_results_resp)
  total_count <- extract_total_count(html)
  search_results <- parse_results_table(html)

  list(
    html = html,
    total_count = total_count,
    search_results = search_results
  )
}

#' Resolve a lookup ID with consistent messaging
#' @noRd
resolve_lookup_id <- function(value, lookup_fn, label) {
  if (is.null(value)) {
    return(NULL)
  }

  id <- lookup_fn(value)
  if (is.null(id)) {
    label_lower <- tolower(label)
    cli::cli_warn(
      "{label} {.val {value}} not found. Search may not filter by {label_lower} correctly."
    )
    return(NULL)
  }

  cli::cli_alert_info("Found {label} ID: {.val {id}}")
  return(id)
}

#' Read and normalize a cached search payload
#' @noRd
get_cached_search_result <- function(
  cache_key,
  ignore_cache = FALSE,
  normalize = identity
) {
  init_cache()

  if (ignore_cache) {
    return(NULL)
  }

  cached_entry <- get_cached(
    tezr_env$search_cache,
    cache_key,
    tezr_env$search_ttl
  )

  normalize(cached_entry)
}

#' Store a search payload in the shared search cache
#' @noRd
set_cached_search_result <- function(cache_key, value, ignore_cache = FALSE) {
  if (!ignore_cache) {
    set_cached(tezr_env$search_cache, cache_key, value)
  }

  invisible(NULL)
}

#' Ensure an HTTP session exists before a search request
#' @noRd
ensure_search_session <- function() {
  if (!has_session()) {
    cli::cli_alert_info("Initializing session...")
    init_session()
  }

  invisible(NULL)
}

#' Run a search request with a consistent status message
#' @noRd
run_search_request <- function(form_data, message) {
  ensure_search_session()
  cli::cli_alert_info(message)

  search_data <- perform_search_request(form_data)

  cli::cli_alert_success("Found {.val {search_data$total_count}} results")
  search_data
}

#' Shared helper for basic search flow
#'
#' Returns a list with `$results` (tibble) and `$total_count` (integer).
#' @noRd
run_basic_search <- function(
  form_data,
  cache_key,
  cache_label,
  ignore_cache = FALSE
) {
  cached_search <- get_cached_search_result(
    cache_key,
    ignore_cache = ignore_cache,
    normalize = normalize_basic_cache_entry
  )

  if (!is.null(cached_search)) {
    cli::cli_alert_success(
      "Returning cached results ({.val {nrow(cached_search$results)}} records)"
    )
    return(list(
      results = cached_search$results,
      total_count = cached_search$total_count
    ))
  }

  search_data <- run_search_request(
    form_data,
    message = paste0("Searching for: ", cache_label)
  )

  total_count <- search_data$total_count
  search_results <- search_data$search_results

  if (total_count == 0) {
    return(list(results = empty_results_tibble(), total_count = 0L))
  }

  set_cached_search_result(
    cache_key,
    list(results = search_results, total_count = total_count),
    ignore_cache = ignore_cache
  )

  if (total_count - nrow(search_results) <= 1) {
    cli::cli_alert_success("Returning {.val {nrow(search_results)}} results")
  }

  return(list(results = search_results, total_count = total_count))
}
