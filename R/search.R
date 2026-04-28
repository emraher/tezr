# Exported search functions for tezr package

#' Search the Turkiye's National Thesis Center
#'
#' Searches the Turkiye's National Thesis Center (Ulusal Tez Merkezi) database
#' and returns matching thesis records.
#'
#' @param keyword Character. The search term(s).
#' @param search_field Character. The field to search in. One of "title",
#'   "author", "supervisor", "subject", "index", "abstract", "all", or "thesis_no".
#'   Default is "all".
#' @param access_type Character. Access type filter. One of "all", "open", or
#'   "restricted". Default is "all".
#' @param thesis_type Character. Type of thesis to search for. One of "all",
#'   "masters", "phd", "medical_specialty", "arts", "dentistry", "medical_sub",
#'   or "pharmacy". Default is "all".
#' @param max_search_results Maximum results to return. Default is 2000 (server
#'   limit per query). Set to `Inf` to automatically delegate to
#'   \code{\link{search_advanced}} for pagination when results exceed
#'   2000.
#' @param ignore_cache Logical. If `TRUE`, bypass cached search/range results and
#'   fetch fresh data from the server.
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
#' @export
#'
#' @examples
#' \dontrun{
#' # Search for theses (returns up to 2000 results)
#' search_results <- search_basic("hanehalkı")
#'
#' # Search for PhD theses by a specific author
#' search_results <- search_basic("Nilay Ünsal", search_field = "author", thesis_type = "masters")
#'
#' # Search for open access theses
#' search_results <- search_basic("tarım", access_type = "open")
#'
#' # Get all results (auto-delegates to advanced search for pagination)
#' all_results <- search_basic("hanehalkı", max_search_results = Inf)
#' }
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
  # Validate arguments
  search_field <- rlang::arg_match(search_field)
  thesis_type <- rlang::arg_match(thesis_type)
  access_type <- rlang::arg_match(access_type)
  validate_ignore_cache(ignore_cache)

  if (
    missing(keyword) ||
      !is.character(keyword) ||
      length(keyword) != 1 ||
      nchar(keyword) == 0
  ) {
    cli::cli_abort("{.arg keyword} must be a single non-empty character string")
  }

  cache_key <- build_search_cache_key(
    type = "basic",
    params = list(
      keyword = keyword,
      search_field = search_field,
      thesis_type = thesis_type,
      access_type = access_type
    )
  )

  form_data <- build_basic_search_form(
    keyword,
    search_field,
    thesis_type,
    access_type
  )

  search_data <- run_basic_search(
    form_data = form_data,
    cache_key = cache_key,
    cache_label = keyword,
    ignore_cache = ignore_cache
  )

  search_results <- search_data$results
  total_count <- search_data$total_count

  search_results <- annotate_search_results(
    search_results,
    total_count = total_count,
    paginated = FALSE
  )

  if (total_count == 0) {
    return(search_results)
  }

  overflow <- total_count - nrow(search_results) > 1

  if (overflow && max_search_results > 2000) {
    cli::cli_alert_info("Delegating to advanced search for pagination...")
    return(search_advanced(
      keyword = keyword,
      search_field = search_field,
      match_type = "contains",
      thesis_type = thesis_type,
      access_type = access_type,
      max_search_results = max_search_results,
      ignore_cache = ignore_cache
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

  return(search_results)
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
  text <- paste(shown, collapse = ", ")
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
    any(!is.na(single_year_overflow_years))

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
      "Set {.arg max_search_results = Inf} to auto-paginate and retrieve all results."
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
  cached_search_results <- get_cached_search_result(
    cache_key,
    ignore_cache = ignore_cache
  )

  if (can_use_cached_search(cached_search_results, requested_all_results)) {
    cli::cli_alert_success(
      "Returning cached results ({.val {nrow(cached_search_results)}} records)"
    )
    if (nrow(cached_search_results) > max_search_results) {
      return(cached_search_results[seq_len(max_search_results), ])
    }
    return(cached_search_results)
  }

  if (!is.null(cached_search_results) && requested_all_results) {
    cli::cli_alert_info(
      "Ignoring cached capped results and refetching complete search output..."
    )
  }

  if (!is.null(prepare_request)) {
    ensure_search_session()
    prepare_request()
  }

  form_data <- form_builder(year_start, year_end)

  search_data <- run_search_request(
    form_data,
    message = paste0("Performing ", search_label, " search...")
  )

  total_count <- search_data$total_count
  search_results <- search_data$search_results

  if (total_count == 0) {
    return(empty_results_tibble())
  }

  single_year_overflow_years <- integer(0)
  paginated <- FALSE

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
    cli::cli_alert_info(
      "Auto-paginating {.val {total_count}} results using year range {.val {year_start}}-{.val {year_end}}"
    )
  }

  if (
    total_count - nrow(search_results) > 1 &&
      nrow(search_results) < max_search_results &&
      !is.null(year_start) &&
      !is.null(year_end)
  ) {
    range_cache_key_params <- if (is.function(cache_key_params)) {
      cache_key_params()
    } else {
      cache_key_params
    }

    search_results <- fetch_by_year_ranges(
      total_count = total_count,
      current_search_results = search_results,
      max_search_results = max_search_results,
      year_start = year_start,
      year_end = year_end,
      form_builder = form_builder,
      cache_key_params = range_cache_key_params,
      ignore_cache = ignore_cache
    )
    paginated <- TRUE
    paginated_overflow_years <- attr(
      search_results,
      "single_year_overflow_years",
      exact = TRUE
    )
    if (!is.null(paginated_overflow_years)) {
      single_year_overflow_years <- paginated_overflow_years
    }
  }

  fetched_count <- nrow(search_results)
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
  } else {
    cli::cli_alert_success("Returning {.val {nrow(search_results)}} results")
  }

  return(search_results)
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
#' @param access_type Character. Access type. One of "all", "open", "restricted".
#'   Default is "all".
#' @param status Character. Thesis status. One of "all", "approved",
#'   "in_preparation". Default is "approved".
#' @param match_type Character. Keyword matching strategy. One of "exact"
#'   (matches the keyword as entered) or "contains" (substring match).
#'   Default is "exact".
#' @param max_search_results Maximum results to return. Default is 2000 (server
#'   limit per query). Use higher values or `Inf` to paginate and retrieve more
#'   results via year-range splitting.
#' @param ignore_cache Logical. If `TRUE`, bypass cached search/range results and
#'   fetch fresh data from the server.
#'
#' @return A tibble containing thesis records (same structure as search_basic).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Keyword search with year filter
#' climate <- search_advanced(
#'   keyword = "iklim değişikliği",
#'   year_start = 2015
#' )
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
#' }
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
  # Validate arguments
  search_field <- rlang::arg_match(search_field)
  thesis_type <- rlang::arg_match(thesis_type)
  access_type <- rlang::arg_match(access_type)
  group <- rlang::arg_match(group)
  status <- rlang::arg_match(status)
  match_type <- rlang::arg_match(match_type)
  validate_ignore_cache(ignore_cache)

  if (
    missing(keyword) ||
      !is.character(keyword) ||
      length(keyword) != 1 ||
      nchar(keyword) == 0
  ) {
    cli::cli_abort("{.arg keyword} must be a single non-empty character string")
  }

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

  cache_key <- build_search_cache_key(
    type = "advanced",
    params = list(
      keyword = keyword,
      search_field = search_field,
      thesis_type = thesis_type,
      access_type = access_type,
      year_start = year_start,
      year_end = year_end,
      language = language,
      group = group,
      university = university,
      university_id = university_id,
      institute = institute,
      institute_id = institute_id,
      status = status,
      match_type = match_type
    )
  )

  prepare_request <- function() {
    if (!is.null(university) && is.null(university_id)) {
      university_id <<- resolve_lookup_id(
        university,
        lookup_university_id,
        "University"
      )
    }

    if (!is.null(institute) && is.null(institute_id)) {
      institute_id <<- resolve_lookup_id(
        institute,
        lookup_institute_id,
        "Institute"
      )
    }
  }

  form_builder <- function(range_start, range_end) {
    build_advanced_search_form(
      keyword = keyword,
      search_field = search_field,
      thesis_type = thesis_type,
      access_type = access_type,
      year_start = range_start,
      year_end = range_end,
      language = language,
      group = group,
      status = status,
      match_type = match_type,
      university = university,
      university_id = university_id,
      institute = institute,
      institute_id = institute_id
    )
  }

  cache_key_params <- function() {
    list(
      search_type = "advanced",
      keyword = keyword,
      search_field = search_field,
      thesis_type = thesis_type,
      access_type = access_type,
      language = language,
      group = group,
      university = university,
      university_id = university_id,
      institute = institute,
      institute_id = institute_id,
      status = status,
      match_type = match_type
    )
  }

  run_search_pipeline(
    search_label = "advanced",
    cache_key = cache_key,
    cache_key_params = cache_key_params,
    form_builder = form_builder,
    year_start = year_start,
    year_end = year_end,
    max_search_results = max_search_results,
    requested_all_results = requested_all_results,
    ignore_cache = ignore_cache,
    prepare_request = prepare_request
  )
}

#' Detailed search of the Turkiye's National Thesis Center
#'
#' Searches with detailed field-specific filter options. When total results exceed 2000
#' (server limit), automatically paginates using year ranges to retrieve
#' all results.
#'
#' @param university University name (optional). Accepts character vector for multiple universities.
#' @param university_id University ID (optional). If provided, lookup by
#'   \code{university} is skipped.
#' @param thesis_type Type(s) of thesis. Default is "all". Accepts character vector
#'   for multiple types: "all", "masters", "phd", "medical_specialty", "arts",
#'   "dentistry", "medical_sub", "pharmacy". Multiple values will trigger
#'   separate searches that are combined.
#' @param year_start Start year (optional). Used for pagination when results > 2000.
#' @param year_end End year (optional). Used for pagination when results > 2000.
#' @param institute Institute name (optional). Accepts character vector for multiple institutes.
#' @param institute_id Institute ID (optional). If provided, lookup by
#'   \code{institute} is skipped.
#' @param access_type Access type. Default is "all". Accepts character vector for
#'   multiple access types: "all", "open", "restricted". Multiple values will
#'   trigger separate searches that are combined.
#' @param group Group filter. One of "all", "science", "social", or
#'   "medical". Default is "all".
#' @param thesis_no Thesis number to search for (optional). Accepts character vector for multiple thesis numbers.
#' @param division Division name (optional). Accepts character vector for multiple divisions.
#' @param division_id Division ID (optional). If provided, lookup by
#'   \code{division} is skipped.
#' @param status Character. Thesis status filter. One of "approved", "all",
#'   "in_preparation". Default is "approved".
#' @param title Title to search for (optional). Accepts character vector for multiple titles.
#' @param discipline Discipline name (optional). Accepts character vector for multiple disciplines.
#' @param discipline_id Discipline ID (optional). If provided, lookup by
#'   \code{discipline} is skipped.
#' @param language Integer language ID or character label (e.g., "tr", "en",
#'   "Turkish", "İngilizce"). Accepts a character vector for multiple languages.
#' @param author Author name (optional). Accepts character vector for multiple authors.
#' @param subject Subject (Konu) name (optional). Accepts character vector for multiple subjects.
#' @param supervisor Supervisor name (optional). Accepts character vector for multiple supervisors.
#' @param keyword Keyword text (Dizin) to search (optional). Accepts character vector
#'   for multiple keywords.
#' @param abstract Abstract text to search (optional). Accepts character vector for multiple abstracts.
#' @param max_search_results Maximum results to return. Default is 2000 (server limit per query).
#'   Use higher values or `Inf` to get all available results via automatic pagination.
#' @param ignore_cache Logical. If `TRUE`, bypass cached search/range results and
#'   fetch fresh data from the server.
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
#' @export
#'
#' @examples
#' \dontrun{
#' # Field-specific search with year filter
#' climate <- search_detailed(
#'   title = "iklim değişikliği",
#'   year_start = 2015,
#'   year_end = 2024
#' )
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
#' search_results <- search_detailed(thesis_no = c("123456", "234567", "345678"))
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
#' }
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
  max_search_results_missing <- missing(max_search_results)

  # Validate thesis_type values
  valid_thesis_types <- c(
    "all",
    "masters",
    "phd",
    "medical_specialty",
    "arts",
    "dentistry",
    "medical_sub",
    "pharmacy"
  )
  if (!all(thesis_type %in% valid_thesis_types)) {
    invalid <- setdiff(thesis_type, valid_thesis_types)
    cli::cli_abort(
      "Invalid {.arg thesis_type} value{?s}: {.val {invalid}}"
    )
  }

  # Validate access_type values
  valid_access_types <- c("all", "open", "restricted")
  if (!all(access_type %in% valid_access_types)) {
    invalid <- setdiff(access_type, valid_access_types)
    cli::cli_abort(
      "Invalid {.arg access_type} value{?s}: {.val {invalid}}"
    )
  }

  valid_groups <- c("all", "science", "social", "medical")
  if (!all(group %in% valid_groups)) {
    invalid <- setdiff(group, valid_groups)
    cli::cli_abort("Invalid {.arg group} value{?s}: {.val {invalid}}")
  }

  # Validate status
  valid_status <- c("all", "approved", "in_preparation")
  if (!status %in% valid_status) {
    cli::cli_abort(
      "Invalid {.arg status} value: {.val {status}}. Must be one of {.val {valid_status}}"
    )
  }
  validate_ignore_cache(ignore_cache)

  # Handle max_search_results = Inf (get all available results)
  requested_all_results <- is.infinite(max_search_results)
  if (requested_all_results) {
    max_search_results <- .Machine$integer.max
  }

  university_id <- validate_optional_id(university_id, "university_id")
  institute_id <- validate_optional_id(institute_id, "institute_id")
  division_id <- validate_optional_id(division_id, "division_id")
  discipline_id <- validate_optional_id(discipline_id, "discipline_id")

  # At least one search criterion or filter must be provided
  has_text_criteria <- !is.null(thesis_no) ||
    !is.null(title) ||
    !is.null(author) ||
    !is.null(supervisor) ||
    !is.null(abstract) ||
    !is.null(keyword)

  has_institution_criteria <- !is.null(university) ||
    !is.null(university_id) ||
    !is.null(institute) ||
    !is.null(institute_id) ||
    !is.null(division) ||
    !is.null(division_id) ||
    !is.null(subject) ||
    !is.null(discipline) ||
    !is.null(discipline_id)

  has_year_criteria <- !is.null(year_start) || !is.null(year_end)

  has_type_criteria <- !is.null(thesis_type) &&
    (length(thesis_type) > 1 || !identical(thesis_type, "all"))

  has_access_criteria <- !is.null(access_type) &&
    (length(access_type) > 1 || !identical(access_type, "all"))

  has_group_criteria <- !is.null(group) &&
    (length(group) > 1 || !identical(group, "all"))

  has_language_criteria <- !is.null(language)

  has_criteria <- has_text_criteria ||
    has_institution_criteria ||
    has_year_criteria ||
    has_type_criteria ||
    has_access_criteria ||
    has_group_criteria ||
    has_language_criteria

  if (!has_criteria) {
    cli::cli_abort("At least one search criterion or filter must be provided")
  }

  year_start <- validate_year(year_start, "year_start")
  year_end <- validate_year(year_end, "year_end")
  if (!is.null(year_start) && !is.null(year_end) && year_start > year_end) {
    cli::cli_abort(
      "{.arg year_start} must be less than or equal to {.arg year_end}"
    )
  }

  # Check if any parameters have multiple values
  multi_params <- list(
    thesis_no = thesis_no,
    title = title,
    author = author,
    supervisor = supervisor,
    abstract = abstract,
    keyword = keyword,
    university = university,
    institute = institute,
    division = division,
    subject = subject,
    discipline = discipline,
    language = language,
    thesis_type = thesis_type,
    access_type = access_type,
    group = group
  )

  # Find which parameters have multiple values
  multi_value_params <- Filter(
    function(x) !is.null(x) && length(x) > 1,
    multi_params
  )

  # If any parameter has multiple values, expand and combine
  if (length(multi_value_params) > 0) {
    return(
      expand_and_search_detailed(
        thesis_no = thesis_no,
        title = title,
        author = author,
        supervisor = supervisor,
        abstract = abstract,
        keyword = keyword,
        university = university,
        university_id = university_id,
        institute = institute,
        institute_id = institute_id,
        division = division,
        division_id = division_id,
        subject = subject,
        discipline = discipline,
        discipline_id = discipline_id,
        thesis_type = thesis_type,
        year_start = year_start,
        year_end = year_end,
        language = language,
        access_type = access_type,
        group = group,
        status = status,
        max_search_results = max_search_results,
        limit_combined_results = !max_search_results_missing,
        ignore_cache = ignore_cache
      )
    )
  }

  cache_key <- build_search_cache_key(
    type = "detailed",
    params = list(
      thesis_no = thesis_no,
      title = title,
      author = author,
      supervisor = supervisor,
      abstract = abstract,
      keyword = keyword,
      university = university,
      university_id = university_id,
      institute = institute,
      institute_id = institute_id,
      division = division,
      division_id = division_id,
      subject = subject,
      discipline = discipline,
      discipline_id = discipline_id,
      thesis_type = thesis_type,
      year_start = year_start,
      year_end = year_end,
      language = language,
      access_type = access_type,
      group = group,
      status = status
    )
  )

  subject_id <- NULL
  prepare_request <- function() {
    if (is.null(university_id)) {
      university_id <<- resolve_lookup_id(
        university,
        lookup_university_id,
        "University"
      )
    }
    if (is.null(institute_id)) {
      institute_id <<- resolve_lookup_id(
        institute,
        lookup_institute_id,
        "Institute"
      )
    }
    if (is.null(division_id)) {
      division_id <<- resolve_lookup_id(
        division,
        lookup_division_id,
        "Division"
      )
    }
    if (is.null(discipline_id)) {
      discipline_id <<- resolve_lookup_id(
        discipline,
        lookup_discipline_id,
        "Discipline"
      )
    }
    subject_id <<- resolve_lookup_id(subject, lookup_subject_id, "Subject")
  }

  form_builder <- function(range_start, range_end) {
    build_detailed_search_form(
      thesis_no = thesis_no,
      title = title,
      author = author,
      supervisor = supervisor,
      abstract = abstract,
      keyword = keyword,
      university = university,
      university_id = university_id,
      institute = institute,
      institute_id = institute_id,
      division = division,
      division_id = division_id,
      subject = subject,
      subject_id = subject_id,
      discipline = discipline,
      discipline_id = discipline_id,
      thesis_type = thesis_type,
      year_start = range_start,
      year_end = range_end,
      language = language,
      access_type = access_type,
      group = group,
      status = status
    )
  }

  cache_key_params <- function() {
    list(
      search_type = "detailed",
      thesis_no = thesis_no,
      title = title,
      author = author,
      supervisor = supervisor,
      abstract = abstract,
      keyword = keyword,
      university_id = university_id,
      institute_id = institute_id,
      division_id = division_id,
      subject_id = subject_id,
      discipline_id = discipline_id,
      thesis_type = thesis_type,
      language = language,
      access_type = access_type,
      group = group,
      status = status
    )
  }

  run_search_pipeline(
    search_label = "detailed",
    cache_key = cache_key,
    cache_key_params = cache_key_params,
    form_builder = form_builder,
    year_start = year_start,
    year_end = year_end,
    max_search_results = max_search_results,
    requested_all_results = requested_all_results,
    ignore_cache = ignore_cache,
    prepare_request = prepare_request
  )
}
