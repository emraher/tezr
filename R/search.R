# Exported search functions for tezr package

#' Validate a search keyword argument
#' @noRd
validate_search_keyword <- function(keyword) {
  if (
    !is.character(keyword) ||
      length(keyword) != 1 ||
      nchar(keyword) == 0
  ) {
    cli::cli_abort("{.arg keyword} must be a single non-empty character string")
  }

  keyword
}

#' Build the cache key for a basic search
#' @noRd
basic_search_cache_key <- function(
  keyword,
  search_field,
  thesis_type,
  access_type
) {
  cache_key <- build_search_cache_key(
    type = "basic",
    params = list(
      keyword = keyword,
      search_field = search_field,
      thesis_type = thesis_type,
      access_type = access_type
    )
  )

  cache_key
}

#' Delegate an overflowing basic search to advanced search
#' @noRd
delegate_basic_search <- function(
  keyword,
  search_field,
  thesis_type,
  access_type,
  max_search_results,
  ignore_cache
) {
  tezr_inform("Delegating to advanced search for pagination...")
  search_advanced(
    keyword = keyword,
    search_field = search_field,
    match_type = "contains",
    thesis_type = thesis_type,
    access_type = access_type,
    max_search_results = max_search_results,
    ignore_cache = ignore_cache
  )
}

#' Return or warn on basic search overflow results
#' @noRd
finalize_basic_search_results <- function(
  search_results,
  total_count,
  keyword,
  search_field,
  thesis_type,
  access_type,
  max_search_results,
  ignore_cache
) {
  if (total_count == 0) {
    return(search_results)
  }

  overflow <- total_count - nrow(search_results) > 1
  if (overflow && max_search_results > 2000) {
    return(delegate_basic_search(
      keyword,
      search_field,
      thesis_type,
      access_type,
      max_search_results,
      ignore_cache
    ))
  }

  if (overflow) {
    cli::cli_alert_warning(
      c(
        "Server limit: returning {.val {nrow(search_results)}} of ",
        "{.val {total_count}} results. ",
        "Set {.arg max_search_results = Inf} to auto-paginate."
      )
    )
  }

  search_results
}

#' Search the Turkiye's National Thesis Center
#'
#' Searches the Turkiye's National Thesis Center (Ulusal Tez Merkezi) database
#' and returns matching thesis records.
#'
#' @param keyword Character. The search term(s).
#' @param search_field Character. The field to search in. One of "title",
#'   "author", "supervisor", "subject", "index", "abstract", "all", or
#'   "thesis_no". Default is "all".
#' @param access_type Character. Access type filter. One of "all", "open", or
#'   "restricted". Default is "all".
#' @param thesis_type Character. Type of thesis to search for. One of "all",
#'   "masters", "phd", "medical_specialty", "arts", "dentistry", "medical_sub",
#'   or "pharmacy". Default is "all".
#' @param max_search_results Maximum results to return. Default is 2000 (server
#'   limit per query). Set to `Inf` to automatically delegate to
#'   \code{\link{search_advanced}} for pagination when results exceed
#'   2000.
#' @param ignore_cache Logical. If `TRUE`, bypass cached search/range results
#'   and fetch fresh data from the server.
#'
#' @details
#' Basic search returns up to 2000 results per query (YOK server limit). When
#' results exceed 2000 and \code{max_search_results > 2000}, the search
#' delegates to \code{\link{search_advanced}} which paginates via
#' year-range splitting to retrieve all results. With the default
#' \code{max_search_results = 2000}, a warning is issued suggesting
#' \code{max_search_results = Inf}.
#'
#' @return A tibble containing thesis records with columns:
#'   \itemize{
#'     \item thesis_no - Unique thesis identifier
#'     \item title_original - Original title
#'     \item title_translation - Title translation when available
#'     \item author - Author name
#'     \item university - University name
#'     \item year - Year of thesis
#'     \item thesis_type_tr - Type of thesis in Turkish
#'     \item thesis_type_en - Type of thesis in English
#'     \item language_tr - Thesis language in Turkish
#'     \item language_en - Thesis language in English
#'     \item subject_tr - Turkish subject classification
#'     \item subject_en - English subject classification
#'     \item detail_id - ID for fetching details
#'   }
#'
#' @family search functions
#' @export
#'
#' @examplesIf interactive()
#' # Search for theses (returns up to 2000 results)
#' search_results <- search_basic("hanehalkı")
#' dplyr::glimpse(search_results)
#' #> Rows: 2,000
#' #> Columns: 13
#' #> $ thesis_no         <chr> "967755", "975988", "955779", ...
#' #> $ title_original    <chr> "Parasal aktarim mekanizmasi...", ...
#' #> $ author            <chr> "PERIHAN EZGI BALLI", ...
#' #> $ university        <chr> "Bandirma Onyedi Eylul Universitesi", ...
#' #> $ year              <int> 2025, 2025, 2025, ...
#' #> $ detail_id         <chr> "TCKf4ksTOVsOBqUcPYMKWQ", ...
#'
#' # Search for PhD theses by a specific author
#' search_results <- search_basic(
#'   "Nilay Ünsal",
#'   search_field = "author",
#'   thesis_type = "masters"
#' )
#'
#' # Search for open access theses
#' search_results <- search_basic("tarım", access_type = "open")
#'
#' # Get all results (auto-delegates to advanced search for pagination)
#' all_results <- search_basic("hanehalkı", max_search_results = Inf)
search_basic <- function(
  keyword,
  search_field = c(
    "all",
    "title",
    "author",
    "supervisor",
    "subject",
    "index",
    "abstract",
    "thesis_no"
  ),
  access_type = c("all", "open", "restricted"),
  thesis_type = c(
    "all",
    "masters",
    "phd",
    "medical_specialty",
    "arts",
    "dentistry",
    "medical_sub",
    "pharmacy"
  ),
  max_search_results = 2000,
  ignore_cache = FALSE
) {
  if (missing(keyword)) {
    cli::cli_abort("{.arg keyword} must be a single non-empty character string")
  }

  args <- normalize_basic_search_args(
    keyword,
    search_field,
    thesis_type,
    access_type,
    max_search_results,
    ignore_cache
  )
  search_data <- execute_basic_search(args)

  finalize_basic_search_results(
    search_data$results,
    search_data$total_count,
    args$keyword,
    args$search_field,
    args$thesis_type,
    args$access_type,
    args$max_search_results,
    args$ignore_cache
  )
}

#' Validate and normalize basic search arguments
#' @noRd
normalize_basic_search_args <- function(
  keyword,
  search_field,
  thesis_type,
  access_type,
  max_search_results,
  ignore_cache
) {
  search_field <- rlang::arg_match(search_field, basic_search_fields())
  thesis_type <- rlang::arg_match(thesis_type, thesis_type_values())
  access_type <- rlang::arg_match(access_type, access_type_values())
  validate_ignore_cache(ignore_cache)

  keyword <- validate_search_keyword(keyword)

  list(
    keyword = keyword,
    search_field = search_field,
    thesis_type = thesis_type,
    access_type = access_type,
    max_search_results = max_search_results,
    ignore_cache = ignore_cache
  )
}

#' Return valid basic search fields
#' @noRd
basic_search_fields <- function() {
  c(
    "all",
    "title",
    "author",
    "supervisor",
    "subject",
    "index",
    "abstract",
    "thesis_no"
  )
}

#' Return valid thesis type values
#' @noRd
thesis_type_values <- function() {
  c(
    "all",
    "masters",
    "phd",
    "medical_specialty",
    "arts",
    "dentistry",
    "medical_sub",
    "pharmacy"
  )
}

#' Return valid access type values
#' @noRd
access_type_values <- function() {
  c("all", "open", "restricted")
}

#' Execute a basic search request and annotate the result tibble
#' @noRd
execute_basic_search <- function(args) {
  form_data <- build_basic_search_form(
    args$keyword,
    args$search_field,
    args$thesis_type,
    args$access_type
  )

  search_data <- run_basic_search(
    form_data = form_data,
    cache_key = basic_search_cache_key(
      args$keyword,
      args$search_field,
      args$thesis_type,
      args$access_type
    ),
    cache_label = args$keyword,
    ignore_cache = args$ignore_cache
  )

  search_data$results <- annotate_search_results(
    search_data$results,
    total_count = search_data$total_count,
    paginated = FALSE
  )

  search_data
}

#' Format year values for user-facing overflow warnings
#' @noRd
format_overflow_years <- function(years, max_years = 6L) {
  years <- sort(unique(as.integer(years)))
  years <- years[!is.na(years)]

  if (length(years) == 0) {
    return("")
  }

  shown <- years[seq_len(min(length(years), max_years))]
  text <- toString(shown)
  remaining <- length(years) - length(shown)

  if (remaining > 0) {
    text <- paste0(text, " and ", remaining, " more")
  }

  return(text)
}

#' Emit consistent warning when returned rows are below reported total
#' @noRd
warn_incomplete_results <- function(
  returned_count,
  total_count,
  requested_all_results,
  single_year_overflow_years = integer(0)
) {
  single_year_overflow_years <- as.integer(single_year_overflow_years)
  has_single_year_overflow <- length(single_year_overflow_years) > 0 &&
    !all(is.na(single_year_overflow_years))

  if (requested_all_results && has_single_year_overflow) {
    year_text <- format_overflow_years(single_year_overflow_years)
    cli::cli_alert_warning(
      c(
        "Returning {.val {returned_count}} of {.val {total_count}} results.",
        paste0(
          "At least one single-year range exceeded the server limit and ",
          "cannot split further (years: ",
          year_text,
          "). Add more ",
          "filters (for example year, thesis type, university, subject)."
        )
      )
    )
    return(invisible(NULL))
  }

  if (requested_all_results) {
    cli::cli_alert_warning(
      c(
        "Returning {.val {returned_count}} of {.val {total_count}} results.",
        paste0(
          "Some results could not be retrieved due to server-side limits or ",
          "response inconsistencies. Review range warnings and add more ",
          "filters to narrow the search."
        )
      )
    )
    return(invisible(NULL))
  }

  cli::cli_alert_warning(
    c(
      "Returning {.val {returned_count}} of {.val {total_count}} results. ",
      paste0(
        "Set {.arg max_search_results = Inf} to auto-paginate and ",
        "retrieve all results."
      )
    )
  )

  return(invisible(NULL))
}

#' Mark search results with cache metadata for later completeness checks
#' @noRd
annotate_search_results <- function(
  results,
  total_count,
  paginated = FALSE,
  single_year_overflow_years = integer(0)
) {
  attr(results, "total_count") <- as.integer(total_count)
  attr(results, "paginated") <- isTRUE(paginated)
  attr(results, "single_year_overflow_years") <- as.integer(
    single_year_overflow_years
  )
  attr(results, "complete") <- isTRUE(total_count - nrow(results) <= 1) &&
    length(single_year_overflow_years) == 0
  results
}

#' Decide whether a cached search result is sufficient for the current request
#' @noRd
can_use_cached_search <- function(results, requested_all_results) {
  if (is.null(results)) {
    return(FALSE)
  }

  if (!requested_all_results) {
    return(TRUE)
  }

  isTRUE(attr(results, "complete", exact = TRUE))
}

#' Run the common search request, cache, pagination, and warning pipeline
#' @noRd
return_cached_search_result <- function(
  cached_search_results,
  max_search_results
) {
  tezr_success(
    "Returning cached results ({.val {nrow(cached_search_results)}} records)"
  )
  if (nrow(cached_search_results) > max_search_results) {
    return(cached_search_results[seq_len(max_search_results), ])
  }

  cached_search_results
}

#' Prepare any lookup-dependent state before search form construction
#' @noRd
prepare_search_pipeline_request <- function(prepare_request = NULL) {
  if (!is.null(prepare_request)) {
    ensure_search_session()
    prepare_request()
  }

  invisible(NULL)
}

#' Resolve the automatic year range used for complete search retrieval
#' @noRd
resolve_auto_year_range <- function(
  total_count,
  max_search_results,
  year_start,
  year_end
) {
  auto_year_range <- FALSE

  if (
    total_count > 2000 &&
      max_search_results > 2000 &&
      is.null(year_start) &&
      is.null(year_end)
  ) {
    year_start <- 1959L
    year_end <- as.integer(format(Sys.Date(), "%Y"))
    auto_year_range <- TRUE
    tezr_inform(paste0(
      "Auto-paginating {.val {total_count}} results using year range ",
      "{.val {year_start}}-{.val {year_end}}"
    ))
  }

  list(
    year_start = year_start,
    year_end = year_end,
    auto_year_range = auto_year_range
  )
}

#' Return whether initial search results need year-range pagination
#' @noRd
needs_year_range_pagination <- function(
  total_count,
  search_results,
  max_search_results,
  year_start,
  year_end
) {
  total_count - nrow(search_results) > 1 &&
    nrow(search_results) < max_search_results &&
    !is.null(year_start) &&
    !is.null(year_end)
}

#' Resolve lazy or literal range-cache parameters
#' @noRd
resolve_range_cache_key_params <- function(cache_key_params) {
  if (is.function(cache_key_params)) {
    return(cache_key_params())
  }

  cache_key_params
}

#' Pick a named subset of an argument list for cache-key construction
#' @noRd
search_args <- function(args, names) {
  selected <- lapply(names, \(name) args[[name]])
  names(selected) <- names
  selected
}

#' Fetch missing rows through year-range pagination
#' @noRd
paginate_pipeline_results <- function(
  total_count,
  search_results,
  max_search_results,
  year_start,
  year_end,
  form_builder,
  cache_key_params,
  ignore_cache
) {
  search_results <- fetch_by_year_ranges(
    total_count = total_count,
    current_search_results = search_results,
    max_search_results = max_search_results,
    year_start = year_start,
    year_end = year_end,
    form_builder = form_builder,
    cache_key_params = resolve_range_cache_key_params(cache_key_params),
    ignore_cache = ignore_cache
  )

  overflow_years <- attr(
    search_results,
    "single_year_overflow_years",
    exact = TRUE
  )
  if (is.null(overflow_years)) {
    overflow_years <- integer(0)
  }

  list(
    search_results = search_results,
    single_year_overflow_years = overflow_years
  )
}

#' Cache, trim, and emit final search pipeline messages
#' @noRd
finish_search_pipeline_results <- function(
  search_results,
  total_count,
  fetched_count,
  paginated,
  auto_year_range,
  single_year_overflow_years,
  cache_key,
  max_search_results,
  requested_all_results,
  ignore_cache
) {
  search_results <- annotate_search_results(
    search_results,
    total_count = total_count,
    paginated = paginated || auto_year_range,
    single_year_overflow_years = single_year_overflow_years
  )

  set_cached_search_result(
    cache_key,
    search_results,
    ignore_cache = ignore_cache
  )

  if (nrow(search_results) > max_search_results) {
    search_results <- search_results[seq_len(max_search_results), ]
  }

  if (total_count - fetched_count > 1) {
    warn_incomplete_results(
      returned_count = nrow(search_results),
      total_count = total_count,
      requested_all_results = requested_all_results,
      single_year_overflow_years = single_year_overflow_years
    )
    return(search_results)
  }

  tezr_success("Returning {.val {nrow(search_results)}} results")
  return(search_results)
}

#' Run the common search request, cache, pagination, and warning pipeline
#' @noRd
run_search_pipeline <- function(
  search_label,
  cache_key,
  cache_key_params,
  form_builder,
  year_start,
  year_end,
  max_search_results,
  requested_all_results,
  ignore_cache,
  prepare_request = NULL
) {
  cached_result <- resolve_cached_search_pipeline_result(
    cache_key,
    max_search_results,
    requested_all_results,
    ignore_cache
  )
  if (!is.null(cached_result)) {
    return(cached_result)
  }

  search_data <- execute_search_pipeline_request(
    search_label,
    form_builder,
    year_start,
    year_end,
    prepare_request
  )
  total_count <- search_data$total_count

  if (total_count == 0) {
    return(empty_results_tibble())
  }

  pagination <- resolve_search_pipeline_pagination(
    total_count,
    search_data$search_results,
    max_search_results,
    year_start,
    year_end,
    form_builder,
    cache_key_params,
    ignore_cache
  )

  finish_search_pipeline_results(
    pagination$search_results,
    total_count,
    fetched_count = nrow(pagination$search_results),
    pagination$paginated,
    pagination$auto_year_range,
    pagination$single_year_overflow_years,
    cache_key,
    max_search_results,
    requested_all_results,
    ignore_cache
  )
}

#' Resolve cached output for the shared search pipeline
#' @noRd
resolve_cached_search_pipeline_result <- function(
  cache_key,
  max_search_results,
  requested_all_results,
  ignore_cache
) {
  cached_search_results <- get_cached_search_result(
    cache_key,
    ignore_cache = ignore_cache
  )

  if (can_use_cached_search(cached_search_results, requested_all_results)) {
    return(return_cached_search_result(
      cached_search_results,
      max_search_results
    ))
  }

  if (!is.null(cached_search_results) && requested_all_results) {
    tezr_inform(
      "Ignoring cached capped results and refetching complete search output..."
    )
  }

  NULL
}

#' Execute the first request in the shared search pipeline
#' @noRd
execute_search_pipeline_request <- function(
  search_label,
  form_builder,
  year_start,
  year_end,
  prepare_request
) {
  prepare_search_pipeline_request(prepare_request)
  form_data <- form_builder(year_start, year_end)

  run_search_request(
    form_data,
    message = paste0("Performing ", search_label, " search...")
  )
}

#' Resolve any year-range pagination required by the search pipeline
#' @noRd
resolve_search_pipeline_pagination <- function(
  total_count,
  search_results,
  max_search_results,
  year_start,
  year_end,
  form_builder,
  cache_key_params,
  ignore_cache
) {
  auto_range <- resolve_auto_year_range(
    total_count,
    max_search_results,
    year_start,
    year_end
  )

  paginate_search_pipeline_results(
    total_count,
    search_results,
    max_search_results,
    auto_range,
    form_builder,
    cache_key_params,
    ignore_cache
  )
}

#' Fetch paginated search pipeline results when a year range is available
#' @noRd
paginate_search_pipeline_results <- function(
  total_count,
  search_results,
  max_search_results,
  auto_range,
  form_builder,
  cache_key_params,
  ignore_cache
) {
  if (
    !needs_year_range_pagination(
      total_count,
      search_results,
      max_search_results,
      auto_range$year_start,
      auto_range$year_end
    )
  ) {
    return(unpaginated_search_pipeline_result(search_results, auto_range))
  }

  paginated_results <- paginate_pipeline_results(
    total_count,
    search_results,
    max_search_results,
    auto_range$year_start,
    auto_range$year_end,
    form_builder,
    cache_key_params,
    ignore_cache
  )

  list(
    search_results = paginated_results$search_results,
    single_year_overflow_years = paginated_results$single_year_overflow_years,
    paginated = TRUE,
    auto_year_range = auto_range$auto_year_range
  )
}

#' Return search pipeline state when pagination is not needed
#' @noRd
unpaginated_search_pipeline_result <- function(search_results, auto_range) {
  list(
    search_results = search_results,
    single_year_overflow_years = integer(0),
    paginated = FALSE,
    auto_year_range = auto_range$auto_year_range
  )
}

#' Advanced search of the Turkiye's National Thesis Center
#'
#' Keyword-based search with common filter options. Similar to basic search
#' but adds year filtering, language, group, institution, and status
#' filters.
#' Limited to 2000 results per request (server limit). For more results or
#' field-specific searches, use \code{\link{search_detailed}}.
#'
#' @param keyword Character. A single search term.
#' @param search_field Character. The field to search in. One of "title",
#'   "author", "supervisor", "subject", "index", "abstract", or "all".
#'   Default is "title" (matches the web advanced search form).
#'
#' @details
#' The YOK portal's advanced search form supports up to three keyword rows
#' combined with Boolean operators (AND, OR, NOT), each targeting a different
#' field. This function exposes only the first keyword row. R packages that
#' interface with academic databases, such as
#' \href{https://docs.ropensci.org/rentrez/}{rentrez} (PubMed) and
#' \href{https://docs.ropensci.org/europepmc/}{europepmc} (Europe PMC), pass
#' Boolean logic as a single query string (e.g., \code{"term1 AND term2"}).
#' The YOK portal does not accept free-form Boolean strings; it uses
#' structured form fields for each keyword row, making that pattern
#' inapplicable here.
#'
#' For equivalent results:
#' \itemize{
#'   \item \strong{AND across fields}: use \code{\link{search_detailed}} with
#'     its field-specific parameters (\code{title}, \code{author},
#'     \code{supervisor}, etc.).
#'   \item \strong{OR}: run separate searches and combine with
#'     \code{dplyr::bind_rows() |> dplyr::distinct()}.
#'   \item \strong{NOT}: run both searches and exclude with
#'     \code{dplyr::anti_join()}.
#' }
#' @param year_start Integer. Start year (optional).
#' @param year_end Integer. End year (optional).
#' @param group Character. Group filter. One of "all",
#'   "science" (Fen), "social" (Sosyal), "medical" (Tıp). Default is "all".
#' @param university Character. University name (optional). If provided without
#'   \code{university_id}, the ID is looked up automatically.
#' @param university_id Integer. University ID (optional). Use this to skip
#'   lookup and match the "Choose" behavior in the web form.
#' @param thesis_type Character. Type of thesis. One of "all", "masters", "phd",
#'   "medical_specialty", "arts", "dentistry", "medical_sub", "pharmacy".
#'   Default is "all".
#' @param institute Character. Institute name (optional). If provided without
#'   \code{institute_id}, the ID is looked up automatically.
#' @param institute_id Integer. Institute ID (optional). Use this to skip
#'   lookup and match the "Choose" behavior in the web form.
#' @param language Integer language ID or character label (e.g., "tr", "en",
#'   "Turkish", "İngilizce").
#' @param access_type Character. Access type. One of "all", "open",
#'   "restricted". Default is "all".
#' @param status Character. Thesis status. One of "all", "approved",
#'   "in_preparation". Default is "approved".
#' @param match_type Character. Keyword matching strategy. One of "exact"
#'   (matches the keyword as entered) or "contains" (substring match).
#'   Default is "exact".
#' @param max_search_results Maximum results to return. Default is 2000 (server
#'   limit per query). Use higher values or `Inf` to paginate and retrieve more
#'   results via year-range splitting.
#' @param ignore_cache Logical. If `TRUE`, bypass cached search/range results
#'   and fetch fresh data from the server.
#'
#' @return A tibble containing thesis records (same structure as search_basic).
#'
#' @family search functions
#' @export
#'
#' @examplesIf interactive()
#' # Keyword search with year filter
#' climate <- search_advanced(
#'   keyword = "iklim değişikliği",
#'   year_start = 2015
#' )
#' dplyr::glimpse(climate)
#' #> Rows: 184
#' #> Columns: 13
#' #> $ thesis_no      <chr> "942101", "931450", "918276", ...
#' #> $ title_original <chr> "Iklim degisikligi...", ...
#' #> $ university     <chr> "Ankara Universitesi", ...
#' #> $ year           <int> 2025, 2024, 2024, ...
#' #> $ thesis_type_en <chr> "Master", "Doctorate", "Master", ...
#'
#' # Search science theses only
#' ml <- search_advanced(
#'   keyword = "makine öğrenmesi",
#'   group = "science",
#'   thesis_type = "phd"
#' )
#'
#' # Search in-preparation theses
#' ongoing <- search_advanced(
#'   keyword = "ekonometri",
#'   status = "in_preparation"
#' )
#'
#' # Filter by university/institute (same fields as web advanced form)
#' odtu <- search_advanced(
#'   keyword = "yapay zeka",
#'   university = "Orta Doğu Teknik Üniversitesi",
#'   institute = "Fen Bilimleri Enstitüsü"
#' )
#'
#' # Get all available results (auto-paginate)
#' all_climate <- search_advanced(
#'   keyword = "iklim değişikliği",
#'   max_search_results = Inf
#' )
search_advanced <- function(
  keyword,
  search_field = c(
    "title",
    "all",
    "author",
    "supervisor",
    "subject",
    "index",
    "abstract"
  ),
  match_type = c("exact", "contains"),
  year_start = NULL,
  year_end = NULL,
  group = c("all", "science", "social", "medical"),
  university = NULL,
  university_id = NULL,
  thesis_type = c(
    "all",
    "masters",
    "phd",
    "medical_specialty",
    "arts",
    "dentistry",
    "medical_sub",
    "pharmacy"
  ),
  institute = NULL,
  institute_id = NULL,
  language = NULL,
  access_type = c("all", "open", "restricted"),
  status = c("approved", "all", "in_preparation"),
  max_search_results = 2000,
  ignore_cache = FALSE
) {
  if (missing(keyword)) {
    cli::cli_abort("{.arg keyword} must be a single non-empty character string")
  }

  args <- normalize_advanced_search_args(
    keyword,
    search_field,
    match_type,
    year_start,
    year_end,
    group,
    university,
    university_id,
    thesis_type,
    institute,
    institute_id,
    language,
    access_type,
    status,
    max_search_results,
    ignore_cache
  )
  resolved_ids <- new_advanced_search_state(args)

  run_search_pipeline(
    search_label = "advanced",
    cache_key = advanced_search_cache_key(args),
    cache_key_params = advanced_cache_key_params(args, resolved_ids),
    form_builder = advanced_form_builder(args, resolved_ids),
    year_start = args$year_start,
    year_end = args$year_end,
    max_search_results = args$max_search_results,
    requested_all_results = args$requested_all_results,
    ignore_cache = args$ignore_cache,
    prepare_request = advanced_prepare_request(args, resolved_ids)
  )
}

#' Validate and normalize advanced search arguments
#' @noRd
normalize_advanced_search_args <- function(
  keyword,
  search_field,
  match_type,
  year_start,
  year_end,
  group,
  university,
  university_id,
  thesis_type,
  institute,
  institute_id,
  language,
  access_type,
  status,
  max_search_results,
  ignore_cache
) {
  search_field <- rlang::arg_match(search_field, advanced_search_fields())
  thesis_type <- rlang::arg_match(thesis_type, thesis_type_values())
  access_type <- rlang::arg_match(access_type, access_type_values())
  group <- rlang::arg_match(group, group_values())
  status <- rlang::arg_match(status, status_values())
  match_type <- rlang::arg_match(match_type, c("exact", "contains"))
  validate_ignore_cache(ignore_cache)
  keyword <- validate_search_keyword(keyword)

  year_start <- validate_year(year_start, "year_start")
  year_end <- validate_year(year_end, "year_end")
  if (!is.null(year_start) && !is.null(year_end) && year_start > year_end) {
    cli::cli_abort(
      "{.arg year_start} must be less than or equal to {.arg year_end}"
    )
  }

  university <- validate_optional_label(university, "university")
  institute <- validate_optional_label(institute, "institute")
  university_id <- validate_optional_id(university_id, "university_id")
  institute_id <- validate_optional_id(institute_id, "institute_id")

  requested_all_results <- is.infinite(max_search_results)
  if (requested_all_results) {
    max_search_results <- .Machine$integer.max
  }

  list(
    keyword = keyword,
    search_field = search_field,
    match_type = match_type,
    year_start = year_start,
    year_end = year_end,
    group = group,
    university = university,
    university_id = university_id,
    thesis_type = thesis_type,
    institute = institute,
    institute_id = institute_id,
    language = language,
    access_type = access_type,
    status = status,
    max_search_results = max_search_results,
    requested_all_results = requested_all_results,
    ignore_cache = ignore_cache
  )
}

#' Return valid advanced search fields
#' @noRd
advanced_search_fields <- function() {
  c(
    "title",
    "all",
    "author",
    "supervisor",
    "subject",
    "index",
    "abstract"
  )
}

#' Return valid group values
#' @noRd
group_values <- function() {
  c("all", "science", "social", "medical")
}

#' Return valid search status values
#' @noRd
status_values <- function() {
  c("all", "approved", "in_preparation")
}

#' Build the cache key for an advanced search
#' @noRd
advanced_search_cache_key <- function(args) {
  build_search_cache_key(
    type = "advanced",
    params = search_args(args, advanced_cache_key_args())
  )
}

#' Return advanced-search argument names included in search cache keys
#' @noRd
advanced_cache_key_args <- function() {
  c(
    "keyword",
    "search_field",
    "thesis_type",
    "access_type",
    "year_start",
    "year_end",
    "language",
    "group",
    "university",
    "university_id",
    "institute",
    "institute_id",
    "status",
    "match_type"
  )
}

#' Create mutable lookup state for an advanced search
#' @noRd
new_advanced_search_state <- function(args) {
  resolved_ids <- new.env(parent = emptyenv())
  resolved_ids$university_id <- args$university_id
  resolved_ids$institute_id <- args$institute_id
  resolved_ids
}

#' Build the lookup-preparation closure for an advanced search
#' @noRd
advanced_prepare_request <- function(args, resolved_ids) {
  function() {
    if (!is.null(args$university) && is.null(resolved_ids$university_id)) {
      resolved_ids$university_id <- resolve_lookup_id(
        args$university,
        lookup_university_id,
        "University"
      )
    }

    if (!is.null(args$institute) && is.null(resolved_ids$institute_id)) {
      resolved_ids$institute_id <- resolve_lookup_id(
        args$institute,
        lookup_institute_id,
        "Institute"
      )
    }
  }
}

#' Build the form closure for an advanced search
#' @noRd
advanced_form_builder <- function(args, resolved_ids) {
  function(range_start, range_end) {
    build_advanced_search_form(
      keyword = args$keyword,
      search_field = args$search_field,
      thesis_type = args$thesis_type,
      access_type = args$access_type,
      year_start = range_start,
      year_end = range_end,
      language = args$language,
      group = args$group,
      status = args$status,
      match_type = args$match_type,
      university = args$university,
      university_id = resolved_ids$university_id,
      institute = args$institute,
      institute_id = resolved_ids$institute_id
    )
  }
}

#' Build the range-cache parameter closure for an advanced search
#' @noRd
advanced_cache_key_params <- function(args, resolved_ids) {
  function() {
    list(
      search_type = "advanced",
      keyword = args$keyword,
      search_field = args$search_field,
      thesis_type = args$thesis_type,
      access_type = args$access_type,
      language = args$language,
      group = args$group,
      university = args$university,
      university_id = resolved_ids$university_id,
      institute = args$institute,
      institute_id = resolved_ids$institute_id,
      status = args$status,
      match_type = args$match_type
    )
  }
}

#' Detailed search of the Turkiye's National Thesis Center
#'
#' Searches with detailed field-specific filter options. When total results
#' exceed 2000 (server limit), automatically paginates using year ranges to
#' retrieve all results.
#'
#' @param university University name (optional). Accepts character vector for
#'   multiple universities.
#' @param university_id University ID (optional). If provided, lookup by
#'   \code{university} is skipped.
#' @param thesis_type Type(s) of thesis. Default is "all". Accepts character
#'   vector for multiple types: "all", "masters", "phd", "medical_specialty",
#'   "arts", "dentistry", "medical_sub", "pharmacy". Multiple values will
#'   trigger separate searches that are combined.
#' @param year_start Start year (optional). Used for pagination when results >
#'   2000.
#' @param year_end End year (optional). Used for pagination when results >
#'   2000.
#' @param institute Institute name (optional). Accepts character vector for
#'   multiple institutes.
#' @param institute_id Institute ID (optional). If provided, lookup by
#'   \code{institute} is skipped.
#' @param access_type Access type. Default is "all". Accepts character vector
#'   for multiple access types: "all", "open", "restricted". Multiple values
#'   will trigger separate searches that are combined.
#' @param group Group filter. One of "all", "science", "social", or
#'   "medical". Default is "all".
#' @param thesis_no Thesis number to search for (optional). Accepts character
#'   vector for multiple thesis numbers.
#' @param division Division name (optional). Accepts character vector for
#'   multiple divisions.
#' @param division_id Division ID (optional). If provided, lookup by
#'   \code{division} is skipped.
#' @param status Character. Thesis status filter. One of "approved", "all",
#'   "in_preparation". Default is "approved".
#' @param title Title to search for (optional). Accepts character vector for
#'   multiple titles.
#' @param discipline Discipline name (optional). Accepts character vector for
#'   multiple disciplines.
#' @param discipline_id Discipline ID (optional). If provided, lookup by
#'   \code{discipline} is skipped.
#' @param language Integer language ID or character label (e.g., "tr", "en",
#'   "Turkish", "İngilizce"). Accepts a character vector for multiple
#'   languages.
#' @param author Author name (optional). Accepts character vector for multiple
#'   authors.
#' @param subject Subject (Konu) name (optional). Accepts character vector for
#'   multiple subjects.
#' @param supervisor Supervisor name (optional). Accepts character vector for
#'   multiple supervisors.
#' @param keyword Keyword text (Dizin) to search (optional). Accepts character
#'   vector for multiple keywords.
#' @param abstract Abstract text to search (optional). Accepts character vector
#'   for multiple abstracts.
#' @param max_search_results Maximum results to return. Default is 2000 (server
#'   limit per query). Use higher values or `Inf` to get all available results
#'   via automatic pagination.
#' @param ignore_cache Logical. If `TRUE`, bypass cached search/range results
#'   and fetch fresh data from the server.
#'
#' @details
#' The YOK web portal accepts only a single value per filter field. This
#' function extends beyond the portal by accepting vector-valued parameters
#' for most fields. Multiple values are expanded into separate API calls,
#' and the results are combined and deduplicated by thesis number.
#'
#' When the total result count exceeds 2000 and no year range is specified,
#' the function automatically uses 1959-present as the year range and paginates
#' to retrieve all results.
#'
#' @return A tibble containing thesis records with columns:
#'   \itemize{
#'     \item thesis_no - Unique thesis identifier
#'     \item title_original - Original title
#'     \item title_translation - Title translation when available
#'     \item author - Author name
#'     \item university - University name
#'     \item year - Year of thesis
#'     \item thesis_type_tr - Type of thesis in Turkish
#'     \item thesis_type_en - Type of thesis in English
#'     \item language_tr - Thesis language in Turkish
#'     \item language_en - Thesis language in English
#'     \item subject_tr - Turkish subject classification
#'     \item subject_en - English subject classification
#'     \item detail_id - ID for fetching details
#'   }
#'
#' @family search functions
#' @export
#'
#' @examplesIf interactive()
#' # Field-specific search with year filter
#' climate <- search_detailed(
#'   title = "iklim değişikliği",
#'   year_start = 2015,
#'   year_end = 2024
#' )
#' head(climate)
#' #> # A tibble: 6 x 13
#' #>   thesis_no title_original author university year thesis_type_en detail_id
#' #>   <chr>     <chr>          <chr>  <chr>      <int> <chr>          <chr>
#' #> 1 967755    Iklim degis... AYSE   Ankara...  2024 Master         TCKf...
#' #> 2 955779    Iklim polit... MEHMET Istanbul...2023 Doctorate      xSXE...
#'
#' # Title search with university filter
#' ml_theses <- search_detailed(
#'   title = "makine öğrenmesi",
#'   university = "Orta Doğu Teknik Üniversitesi",
#'   thesis_type = "phd"
#' )
#'
#' # Search by university and division
#' search_results <- search_detailed(
#'   university = "Ankara Üniversitesi",
#'   division = "İktisat Ana Bilim Dalı",
#'   year_start = 2020,
#'   year_end = 2023
#' )
#'
#' # Get ALL results for a subject (auto-paginates if > 2000)
#' all_econ <- search_detailed(subject = "Ekonometri", max_search_results = Inf)
#'
#' # Search for a specific supervisor's theses
#' search_results <- search_detailed(supervisor = "Hasan Şahin")
#'
#' # Search across multiple universities (automatically expands)
#' search_results <- search_detailed(
#'   university = c("Ankara Üniversitesi", "İstanbul Üniversitesi"),
#'   subject = "Ekonomi"
#' )
#'
#' # Search multiple disciplines within a subject
#' search_results <- search_detailed(
#'   subject = "Ekonomi",
#'   discipline = c("İktisat", "Maliye", "Ekonometri")
#' )
#'
#' # Search for multiple specific thesis numbers
#' search_results <- search_detailed(
#'   thesis_no = c("123456", "234567", "345678")
#' )
#'
#' # Search for multiple titles
#' search_results <- search_detailed(
#'   title = c("Ekonometri", "Zaman Serileri")
#' )
#'
#' # Search for multiple languages
#' search_results <- search_detailed(
#'   subject = "Ekonomi",
#'   language = c("tr", "en")
#' )
#'
#' # Search for multiple thesis types
#' search_results <- search_detailed(
#'   subject = "Ekonomi",
#'   thesis_type = c("phd", "masters")
#' )
#'
#' # Search for multiple access types
#' search_results <- search_detailed(
#'   subject = "Makine Mühendisliği",
#'   access_type = c("open", "restricted")
#' )
#'
#' # Complex multi-parameter search
#' search_results <- search_detailed(
#'   author = c("Ahmet Yılmaz", "Mehmet Demir"),
#'   university = c("Ankara Üniversitesi", "İstanbul Üniversitesi"),
#'   year_start = 2020
#' )
search_detailed <- function(
  university = NULL,
  university_id = NULL,
  thesis_type = "all",
  year_start = NULL,
  year_end = NULL,
  institute = NULL,
  institute_id = NULL,
  access_type = "all",
  group = "all",
  thesis_no = NULL,
  division = NULL,
  division_id = NULL,
  status = "approved",
  title = NULL,
  discipline = NULL,
  discipline_id = NULL,
  language = NULL,
  author = NULL,
  subject = NULL,
  supervisor = NULL,
  keyword = NULL,
  abstract = NULL,
  max_search_results = 2000,
  ignore_cache = FALSE
) {
  args <- normalize_detailed_search_args(
    university = university,
    university_id = university_id,
    thesis_type = thesis_type,
    year_start = year_start,
    year_end = year_end,
    institute = institute,
    institute_id = institute_id,
    access_type = access_type,
    group = group,
    thesis_no = thesis_no,
    division = division,
    division_id = division_id,
    status = status,
    title = title,
    discipline = discipline,
    discipline_id = discipline_id,
    language = language,
    author = author,
    subject = subject,
    supervisor = supervisor,
    keyword = keyword,
    abstract = abstract,
    max_search_results = max_search_results,
    limit_combined_results = !missing(max_search_results),
    ignore_cache = ignore_cache
  )

  if (!has_detailed_search_criteria(args)) {
    cli::cli_abort("At least one search criterion or filter must be provided")
  }

  if (has_multi_value_detailed_args(args)) {
    return(expand_detailed_search(args))
  }

  run_single_detailed_search(args)
}

#' Run a detailed search after validation and expansion checks
#' @noRd
run_single_detailed_search <- function(args) {
  resolved_ids <- new_detailed_search_state(args)

  run_search_pipeline(
    search_label = "detailed",
    cache_key = detailed_search_cache_key(args),
    cache_key_params = detailed_cache_key_params(args, resolved_ids),
    form_builder = detailed_form_builder(args, resolved_ids),
    year_start = args$year_start,
    year_end = args$year_end,
    max_search_results = args$max_search_results,
    requested_all_results = args$requested_all_results,
    ignore_cache = args$ignore_cache,
    prepare_request = detailed_prepare_request(args, resolved_ids)
  )
}

#' Validate and normalize detailed search arguments
#' @noRd
normalize_detailed_search_args <- function(
  university,
  university_id,
  thesis_type,
  year_start,
  year_end,
  institute,
  institute_id,
  access_type,
  group,
  thesis_no,
  division,
  division_id,
  status,
  title,
  discipline,
  discipline_id,
  language,
  author,
  subject,
  supervisor,
  keyword,
  abstract,
  max_search_results,
  limit_combined_results,
  ignore_cache
) {
  validate_detailed_enum_values(thesis_type, access_type, group, status)
  validate_ignore_cache(ignore_cache)
  limit <- normalize_search_result_limit(max_search_results)
  years <- validate_search_year_range(year_start, year_end)
  ids <- validate_detailed_ids(
    university_id,
    institute_id,
    division_id,
    discipline_id
  )

  list(
    university = university,
    university_id = ids$university_id,
    thesis_type = thesis_type,
    year_start = years$year_start,
    year_end = years$year_end,
    institute = institute,
    institute_id = ids$institute_id,
    access_type = access_type,
    group = group,
    thesis_no = thesis_no,
    division = division,
    division_id = ids$division_id,
    status = status,
    title = title,
    discipline = discipline,
    discipline_id = ids$discipline_id,
    language = language,
    author = author,
    subject = subject,
    supervisor = supervisor,
    keyword = keyword,
    abstract = abstract,
    max_search_results = limit$max_search_results,
    requested_all_results = limit$requested_all_results,
    limit_combined_results = limit_combined_results,
    ignore_cache = ignore_cache
  )
}

#' Normalize result-limit arguments shared by search helpers
#' @noRd
normalize_search_result_limit <- function(max_search_results) {
  requested_all_results <- is.infinite(max_search_results)
  if (requested_all_results) {
    max_search_results <- .Machine$integer.max
  }

  list(
    max_search_results = max_search_results,
    requested_all_results = requested_all_results
  )
}

#' Validate detailed search enum arguments
#' @noRd
validate_detailed_enum_values <- function(
  thesis_type,
  access_type,
  group,
  status
) {
  abort_invalid_values(
    thesis_type,
    thesis_type_values(),
    "Invalid {.arg thesis_type} value{?s}: {.val {invalid}}"
  )
  abort_invalid_values(
    access_type,
    access_type_values(),
    "Invalid {.arg access_type} value{?s}: {.val {invalid}}"
  )
  abort_invalid_values(
    group,
    group_values(),
    "Invalid {.arg group} value{?s}: {.val {invalid}}"
  )
  abort_invalid_status(status)
}

#' Abort when values are outside an allowed set
#' @noRd
abort_invalid_values <- function(values, valid_values, message) {
  if (all(values %in% valid_values)) {
    return(invisible(NULL))
  }

  invalid <- setdiff(values, valid_values)
  cli::cli_abort(message)
}

#' Abort when detailed-search status is invalid
#' @noRd
abort_invalid_status <- function(status) {
  valid_status <- status_values()
  if (all(status %in% valid_status)) {
    return(invisible(NULL))
  }

  cli::cli_abort(
    paste0(
      "Invalid {.arg status} value: {.val {status}}. ",
      "Must be one of {.val {valid_status}}"
    )
  )
}

#' Validate search year boundaries
#' @noRd
validate_search_year_range <- function(year_start, year_end) {
  year_start <- validate_year(year_start, "year_start")
  year_end <- validate_year(year_end, "year_end")

  if (!is.null(year_start) && !is.null(year_end) && year_start > year_end) {
    cli::cli_abort(
      "{.arg year_start} must be less than or equal to {.arg year_end}"
    )
  }

  list(year_start = year_start, year_end = year_end)
}

#' Validate optional detailed-search ID arguments
#' @noRd
validate_detailed_ids <- function(
  university_id,
  institute_id,
  division_id,
  discipline_id
) {
  list(
    university_id = validate_optional_id(university_id, "university_id"),
    institute_id = validate_optional_id(institute_id, "institute_id"),
    division_id = validate_optional_id(division_id, "division_id"),
    discipline_id = validate_optional_id(discipline_id, "discipline_id")
  )
}

#' Return whether detailed search has at least one criterion
#' @noRd
has_detailed_search_criteria <- function(args) {
  any(detailed_search_criteria(args))
}

#' Return detailed-search criterion flags
#' @noRd
detailed_search_criteria <- function(args) {
  c(
    any_non_null_arg(args, detailed_text_args()),
    any_non_null_arg(args, detailed_institution_args()),
    has_detailed_year_criteria(args),
    has_detailed_filter_criteria(args),
    !is.null(args$language)
  )
}

#' Return whether detailed search has a year criterion
#' @noRd
has_detailed_year_criteria <- function(args) {
  !is.null(args$year_start) || !is.null(args$year_end)
}

#' Return whether detailed search has a non-default filter criterion
#' @noRd
has_detailed_filter_criteria <- function(args) {
  any(vapply(
    c("thesis_type", "access_type", "group"),
    function(arg_name) is_non_default_all_filter(args[[arg_name]]),
    FALSE
  ))
}

#' Return detailed-search text criterion names
#' @noRd
detailed_text_args <- function() {
  c("thesis_no", "title", "author", "supervisor", "abstract", "keyword")
}

#' Return detailed-search institution criterion names
#' @noRd
detailed_institution_args <- function() {
  c(
    "university",
    "university_id",
    "institute",
    "institute_id",
    "division",
    "division_id",
    "subject",
    "discipline",
    "discipline_id"
  )
}

#' Return whether any named argument is non-null
#' @noRd
any_non_null_arg <- function(args, arg_names) {
  any(vapply(arg_names, function(arg_name) !is.null(args[[arg_name]]), FALSE))
}

#' Return whether an "all" filter is active
#' @noRd
is_non_default_all_filter <- function(value) {
  !is.null(value) && (length(value) > 1 || !identical(value, "all"))
}

#' Return whether detailed search must expand vector-valued inputs
#' @noRd
has_multi_value_detailed_args <- function(args) {
  any(vapply(
    detailed_multi_value_args(args),
    function(value) !is.null(value) && length(value) > 1,
    FALSE
  ))
}

#' Return detailed-search arguments that support multi-value expansion
#' @noRd
detailed_multi_value_args <- function(args) {
  args[c(
    "thesis_no",
    "title",
    "author",
    "supervisor",
    "abstract",
    "keyword",
    "university",
    "institute",
    "division",
    "subject",
    "discipline",
    "language",
    "thesis_type",
    "access_type",
    "group"
  )]
}

#' Expand and combine a vector-valued detailed search
#' @noRd
expand_detailed_search <- function(args) {
  expand_and_search_detailed(
    thesis_no = args$thesis_no,
    title = args$title,
    author = args$author,
    supervisor = args$supervisor,
    abstract = args$abstract,
    keyword = args$keyword,
    university = args$university,
    university_id = args$university_id,
    institute = args$institute,
    institute_id = args$institute_id,
    division = args$division,
    division_id = args$division_id,
    subject = args$subject,
    discipline = args$discipline,
    discipline_id = args$discipline_id,
    thesis_type = args$thesis_type,
    year_start = args$year_start,
    year_end = args$year_end,
    language = args$language,
    access_type = args$access_type,
    group = args$group,
    status = args$status,
    max_search_results = args$max_search_results,
    limit_combined_results = args$limit_combined_results,
    ignore_cache = args$ignore_cache
  )
}

#' Build the cache key for a detailed search
#' @noRd
detailed_search_cache_key <- function(args) {
  build_search_cache_key(
    type = "detailed",
    params = search_args(args, detailed_cache_key_args())
  )
}

#' Return detailed-search argument names included in search cache keys
#' @noRd
detailed_cache_key_args <- function() {
  c(
    "thesis_no",
    "title",
    "author",
    "supervisor",
    "abstract",
    "keyword",
    "university",
    "university_id",
    "institute",
    "institute_id",
    "division",
    "division_id",
    "subject",
    "discipline",
    "discipline_id",
    "thesis_type",
    "year_start",
    "year_end",
    "language",
    "access_type",
    "group",
    "status"
  )
}

#' Create mutable lookup state for a detailed search
#' @noRd
new_detailed_search_state <- function(args) {
  resolved_ids <- new.env(parent = emptyenv())
  resolved_ids$university_id <- args$university_id
  resolved_ids$institute_id <- args$institute_id
  resolved_ids$division_id <- args$division_id
  resolved_ids$discipline_id <- args$discipline_id
  resolved_ids$subject_id <- NULL
  resolved_ids
}

#' Build the lookup-preparation closure for a detailed search
#' @noRd
detailed_prepare_request <- function(args, resolved_ids) {
  function() {
    if (is.null(resolved_ids$university_id)) {
      resolved_ids$university_id <- resolve_lookup_id(
        args$university,
        lookup_university_id,
        "University"
      )
    }
    if (is.null(resolved_ids$institute_id)) {
      resolved_ids$institute_id <- resolve_lookup_id(
        args$institute,
        lookup_institute_id,
        "Institute"
      )
    }
    if (is.null(resolved_ids$division_id)) {
      resolved_ids$division_id <- resolve_lookup_id(
        args$division,
        lookup_division_id,
        "Division"
      )
    }
    if (is.null(resolved_ids$discipline_id)) {
      resolved_ids$discipline_id <- resolve_lookup_id(
        args$discipline,
        lookup_discipline_id,
        "Discipline"
      )
    }
    resolved_ids$subject_id <- resolve_lookup_id(
      args$subject,
      lookup_subject_id,
      "Subject"
    )
  }
}

#' Build the form closure for a detailed search
#' @noRd
detailed_form_builder <- function(args, resolved_ids) {
  function(range_start, range_end) {
    build_detailed_search_form(
      thesis_no = args$thesis_no,
      title = args$title,
      author = args$author,
      supervisor = args$supervisor,
      abstract = args$abstract,
      keyword = args$keyword,
      university = args$university,
      university_id = resolved_ids$university_id,
      institute = args$institute,
      institute_id = resolved_ids$institute_id,
      division = args$division,
      division_id = resolved_ids$division_id,
      subject = args$subject,
      subject_id = resolved_ids$subject_id,
      discipline = args$discipline,
      discipline_id = resolved_ids$discipline_id,
      thesis_type = args$thesis_type,
      year_start = range_start,
      year_end = range_end,
      language = args$language,
      access_type = args$access_type,
      group = args$group,
      status = args$status
    )
  }
}

#' Build the range-cache parameter closure for a detailed search
#' @noRd
detailed_cache_key_params <- function(args, resolved_ids) {
  function() {
    list(
      search_type = "detailed",
      thesis_no = args$thesis_no,
      title = args$title,
      author = args$author,
      supervisor = args$supervisor,
      abstract = args$abstract,
      keyword = args$keyword,
      university_id = resolved_ids$university_id,
      institute_id = resolved_ids$institute_id,
      division_id = resolved_ids$division_id,
      subject_id = resolved_ids$subject_id,
      discipline_id = resolved_ids$discipline_id,
      thesis_type = args$thesis_type,
      language = args$language,
      access_type = args$access_type,
      group = args$group,
      status = args$status
    )
  }
}
