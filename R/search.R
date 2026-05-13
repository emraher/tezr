# Exported search functions for tezr package

#' Search the Turkiye's National Thesis Center
#'
#' Searches the Turkiye's National Thesis Center (Ulusal Tez Merkezi) database
#' and returns matching thesis records.
#'
#' @param keyword Character. The search term(s).
#' @param search_field Character. The field to search in. One of "title",
#'   "author", "supervisor", "subject", "index", "abstract", or "all".
#'   Default is "all". The legacy `"thesis_no"` value is not supported by
#'   YOK's keyword endpoint. Use `search_detailed(thesis_no = ...)` instead.
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
#' year-range splitting to retrieve more results. Single-year ranges can still
#' be capped by the server. With the default \code{max_search_results = 2000},
#' a warning is issued suggesting \code{max_search_results = Inf}.
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
#' # Paginate beyond the first server batch when possible
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
  max_search_results <- validate_max_search_results(max_search_results)

  if (identical(search_field, "thesis_no")) {
    cli::cli_abort(
      paste0(
        "YOK's keyword endpoint does not support thesis number search ",
        "reliably. Use {.code search_detailed(thesis_no = ...)} instead."
      )
    )
  }

  if (missing(keyword)) {
    cli::cli_abort("{.arg keyword} must be a single non-empty character string")
  }
  keyword <- validate_text_values(keyword, "keyword", allow_multiple = FALSE)

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
  fetched_count <- nrow(search_results)

  search_results <- annotate_search_results(
    search_results,
    total_count = total_count,
    paginated = FALSE
  )

  if (total_count == 0) {
    return(search_results)
  }

  overflow <- total_count - fetched_count > 1

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

  if (nrow(search_results) > max_search_results) {
    search_results <- search_results[seq_len(max_search_results), ]
  }

  if (!overflow) {
    cli::cli_alert_success("Returning {.val {nrow(search_results)}} results")
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
        "Returning {.val {returned_count}} of {.val {total_count}} results. ",
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
        "Returning {.val {returned_count}} of {.val {total_count}} results. ",
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
      "Set {.arg max_search_results = Inf} to auto-paginate beyond the first server batch."
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

#' Validate detailed-search vector choices
#' @noRd
validate_detailed_choices <- function(values, arg_name, valid_values) {
  if (!is.character(values) || length(values) == 0L || any(is.na(values))) {
    cli::cli_abort("{.arg {arg_name}} must contain one or more valid values")
  }

  invalid_values <- setdiff(values, valid_values)
  if (length(invalid_values) > 0L) {
    cli::cli_abort(
      "Invalid {.arg {arg_name}} value{?s}: {.val {invalid_values}}"
    )
  }

  values
}

#' Decide whether a detailed university query can be retried locally
#' @noRd
has_university_fallback_filter <- function(
  thesis_no = NULL,
  title = NULL,
  author = NULL,
  supervisor = NULL,
  abstract = NULL,
  keyword = NULL,
  subject = NULL,
  institute = NULL,
  institute_id = NULL,
  division = NULL,
  division_id = NULL,
  discipline = NULL,
  discipline_id = NULL,
  year_start = NULL,
  year_end = NULL,
  language = NULL,
  group = "all",
  thesis_type = "all",
  access_type = "all",
  status = "approved"
) {
  is_non_default <- function(value, default) {
    !is.null(value) &&
      !(length(value) == 1L && identical(value[[1L]], default))
  }

  any(vapply(
    list(
      thesis_no,
      title,
      author,
      supervisor,
      abstract,
      keyword,
      subject,
      institute,
      institute_id,
      division,
      division_id,
      discipline,
      discipline_id,
      year_start,
      year_end,
      language,
      if (is_non_default(group, "all")) group else NULL,
      if (is_non_default(thesis_type, "all")) thesis_type else NULL,
      if (is_non_default(access_type, "all")) access_type else NULL,
      if (is_non_default(status, "approved")) status else NULL
    ),
    function(value) !is.null(value),
    logical(1)
  ))
}

#' Filter search rows by canonical university label
#' @noRd
filter_search_results_by_university <- function(search_results, university) {
  if (
    is.null(university) ||
      nrow(search_results) == 0 ||
      !"university" %in% names(search_results)
  ) {
    return(search_results[0, , drop = FALSE])
  }

  target <- normalize_lookup_label(university)
  row_universities <- normalize_lookup_label(search_results$university)
  keep <- !is.na(row_universities) &
    nzchar(row_universities) &
    (
      row_universities == target |
        stringr::str_detect(row_universities, stringr::fixed(target)) |
        stringr::str_detect(target, stringr::fixed(row_universities))
    )

  filtered_results <- search_results[keep, , drop = FALSE]
  source_overflow_years <- attr(
    search_results,
    "single_year_overflow_years",
    exact = TRUE
  )
  if (is.null(source_overflow_years)) {
    source_overflow_years <- integer(0)
  }

  filtered_results <- annotate_search_results(
    filtered_results,
    total_count = nrow(filtered_results),
    paginated = isTRUE(attr(search_results, "paginated", exact = TRUE)),
    single_year_overflow_years = source_overflow_years
  )
  attr(filtered_results, "complete") <- isTRUE(
    attr(search_results, "complete", exact = TRUE)
  )

  filtered_results
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
    cached_count <- nrow(cached_search_results)
    if (nrow(cached_search_results) > max_search_results) {
      cached_search_results <- cached_search_results[seq_len(max_search_results), ]
      cli::cli_alert_success(
        sprintf(
          "Returning cached results (%d of %d records)",
          nrow(cached_search_results),
          cached_count
        )
      )
      return(cached_search_results)
    }
    cli::cli_alert_success(
      sprintf("Returning cached results (%d records)", cached_count)
    )
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
#' but adds year filtering, language, university, institute, group, and status
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
#' University and group filters are sent with the keyword endpoint. Institute
#' filters are sent through the detailed form for field-specific searches
#' because YOK's all-field keyword endpoint ignores institute values.
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
#' @param group Character. Group filter. One of "all", "science", "social", or
#'   "medical". Default is "all".
#' @param university Character. University name (optional). If provided without
#'   \code{university_id}, the ID is looked up automatically.
#' @param university_id Integer. University ID (optional). Use this to skip
#'   lookup.
#' @param thesis_type Character. Type of thesis. One of "all", "masters", "phd",
#'   "medical_specialty", "arts", "dentistry", "medical_sub", "pharmacy".
#'   Default is "all".
#' @param institute Character. Institute name (optional). If provided without
#'   \code{institute_id}, the ID is looked up automatically. Institute filters
#'   require a field-specific `search_field`.
#' @param institute_id Integer. Institute ID (optional). Use this to skip
#'   lookup. Institute filters require a field-specific `search_field`.
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
#' # Search PhD theses only
#' ml <- search_advanced(
#'   keyword = "makine öğrenmesi",
#'   thesis_type = "phd"
#' )
#'
#' # Search in-preparation theses
#' ongoing <- search_advanced(
#'   keyword = "ekonometri",
#'   status = "in_preparation"
#' )
#'
#' # Paginate a broad query
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
  max_search_results <- validate_max_search_results(max_search_results)

  if (missing(keyword)) {
    cli::cli_abort("{.arg keyword} must be a single non-empty character string")
  }
  keyword <- validate_text_values(keyword, "keyword", allow_multiple = FALSE)

  year_start <- validate_year(year_start, "year_start")
  year_end <- validate_year(year_end, "year_end")

  if (!is.null(year_start) && !is.null(year_end) && year_start > year_end) {
    cli::cli_abort(
      "{.arg year_start} must be less than or equal to {.arg year_end}"
    )
  }
  language <- validate_language_values(language, allow_multiple = FALSE)

  university <- validate_optional_label(university, "university")
  institute <- validate_optional_label(institute, "institute")
  university_id <- validate_optional_id(university_id, "university_id")
  institute_id <- validate_optional_id(institute_id, "institute_id")

  requested_all_results <- is.infinite(max_search_results)
  if (requested_all_results) {
    max_search_results <- .Machine$integer.max
  }

  prepare_request <- function() {
    if (!is.null(university) && is.null(university_id)) {
      matched_university <- resolve_lookup_item(
        university,
        lookup_university_item,
        "University"
      )
      if (!is.null(matched_university)) {
        university_id <<- matched_university$id
        university <<- matched_university$name
      }
    }

    if (!is.null(institute) && is.null(institute_id)) {
      matched_institute <- resolve_lookup_item(
        institute,
        lookup_institute_item,
        "Institute"
      )
      if (!is.null(matched_institute)) {
        institute_id <<- matched_institute$id
        institute <<- matched_institute$name
      }
    }
  }

  use_detailed_form <- !is.null(institute) || !is.null(institute_id)

  if (use_detailed_form && identical(search_field, "all")) {
    cli::cli_abort(
      paste0(
        "YOK's all-field keyword endpoint ignores institute filters. ",
        "Use {.fn search_detailed} for institute-only searches or set ",
        "{.arg search_field} to a specific field."
      )
    )
  }

  if (use_detailed_form) {
    if (!identical(match_type, "contains")) {
      cli::cli_alert_warning(
        paste0(
          "YOK applies institute filters through the detailed form, which ",
          "does not expose exact keyword matching. Using the detailed form's ",
          "text matching."
        )
      )
    }

    detailed_field <- switch(
      search_field,
      title = "title",
      author = "author",
      supervisor = "supervisor",
      subject = "subject",
      index = "keyword",
      abstract = "abstract"
    )

    cache_key <- build_search_cache_key(
      type = "advanced_detailed",
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
        status = status
      )
    )

    form_builder <- function(range_start, range_end) {
      form_args <- list(
        thesis_type = thesis_type,
        access_type = access_type,
        year_start = range_start,
        year_end = range_end,
        language = language,
        group = group,
        status = status,
        university = university,
        university_id = university_id,
        institute = institute,
        institute_id = institute_id
      )
      form_args[[detailed_field]] <- keyword
      do.call(build_detailed_search_form, form_args)
    }

    cache_key_params <- function() {
      list(
        search_type = "advanced_detailed",
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
        status = status
      )
    }

    return(run_search_pipeline(
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
    ))
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
      status = status,
      match_type = match_type
    )
  )

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
      university_id = university_id
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
#' Searches YOK's redesigned detailed form. When total results exceed 2000,
#' automatically paginates using year ranges to retrieve more results. Very
#' broad single-year ranges can still be capped by the server.
#'
#' @param university University name filter. If provided without
#'   \code{university_id}, the ID is looked up automatically.
#' @param university_id University ID filter. Use this to skip lookup.
#' @param thesis_type Type(s) of thesis. Default is "all". Accepts character vector
#'   for multiple types: "all", "masters", "phd", "medical_specialty", "arts",
#'   "dentistry", "medical_sub", "pharmacy". Multiple values will trigger
#'   separate searches that are combined.
#' @param year_start Start year (optional). Used for pagination when results > 2000.
#' @param year_end End year (optional). Used for pagination when results > 2000.
#' @param institute Institute name filter. If provided without
#'   \code{institute_id}, the ID is looked up automatically.
#' @param institute_id Institute ID filter. Use this to skip lookup.
#' @param access_type Access type. Default is "all". Accepts character vector for
#'   multiple access types: "all", "open", "restricted". Multiple values will
#'   trigger separate searches that are combined.
#' @param group Group filter. One of "all", "science", "social", or "medical".
#' @param thesis_no Thesis number to search for. This uses YOK's detailed form
#'   endpoint because the redesigned keyword endpoint is unreliable for thesis
#'   numbers.
#' @param division Division name filter. If provided without
#'   \code{division_id}, the ID is looked up automatically.
#' @param division_id Division ID filter. Use this to skip lookup.
#' @param status Character. Thesis status filter. One of "approved", "all",
#'   "in_preparation". Default is "approved".
#' @param title Title to search for (optional). Accepts character vector for multiple titles.
#' @param discipline Discipline name filter. If provided without
#'   \code{discipline_id}, the ID is looked up automatically.
#' @param discipline_id Discipline ID filter. Use this to skip lookup.
#' @param language Integer language ID or character label (e.g., "tr", "en",
#'   "Turkish", "İngilizce"). Accepts a character vector for multiple languages.
#' @param author Author name (optional). Accepts character vector for multiple authors.
#' @param subject Subject (Konu) name (optional). Accepts character vector for multiple subjects.
#' @param supervisor Supervisor name (optional). Accepts character vector for multiple supervisors.
#' @param keyword Keyword text (Dizin) to search (optional). Accepts character vector
#'   for multiple keywords.
#' @param abstract Abstract text to search (optional). Accepts character vector for multiple abstracts.
#' @param max_search_results Maximum results to return. Default is 2000 (server limit per query).
#'   Use higher values or `Inf` to paginate beyond the first server batch when
#'   year-range splitting can narrow each request below the server cap.
#' @param ignore_cache Logical. If `TRUE`, bypass cached search/range results and
#'   fetch fresh data from the server.
#'
#' @details
#' The detailed form accepts field-specific and institutional filters in the
#' same request, including `title`, `author`, `supervisor`, `subject`,
#' `keyword`, `abstract`, `thesis_no`, `university`, `institute`, `division`,
#' `discipline`, and `group`. Vector-valued fields are expanded into separate
#' requests, and the results are combined and deduplicated by thesis number.
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
#' # Title search with a thesis type filter
#' ml_theses <- search_detailed(
#'   title = "makine öğrenmesi",
#'   thesis_type = "phd"
#' )
#'
#' # Search by subject and year range
#' search_results <- search_detailed(
#'   subject = "Ekonometri",
#'   year_start = 2020,
#'   year_end = 2023
#' )
#'
#' # Paginate a broad subject search
#' all_econ <- search_detailed(subject = "Ekonometri", max_search_results = Inf)
#'
#' # Search for a specific supervisor's theses
#' search_results <- search_detailed(supervisor = "Hasan Şahin")
#'
#' # Search for a specific thesis number
#' search_results <- search_detailed(thesis_no = "12345")
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
  max_search_results <- validate_max_search_results(max_search_results)

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
  thesis_type <- validate_detailed_choices(
    thesis_type,
    "thesis_type",
    valid_thesis_types
  )

  # Validate access_type values
  valid_access_types <- c("all", "open", "restricted")
  access_type <- validate_detailed_choices(
    access_type,
    "access_type",
    valid_access_types
  )

  valid_groups <- c("all", "science", "social", "medical")
  group <- validate_detailed_choices(group, "group", valid_groups)

  # Validate status
  valid_status <- c("all", "approved", "in_preparation")
  if (
    !is.character(status) ||
      length(status) != 1L ||
      is.na(status) ||
      !status %in% valid_status
  ) {
    cli::cli_abort(
      "Invalid {.arg status} value: {.val {status}}. Must be one of {.val {valid_status}}"
    )
  }
  validate_ignore_cache(ignore_cache)

  # Handle max_search_results = Inf (paginate beyond the first server batch)
  requested_all_results <- is.infinite(max_search_results)
  if (requested_all_results) {
    max_search_results <- .Machine$integer.max
  }

  university_id <- validate_optional_id(university_id, "university_id")
  institute_id <- validate_optional_id(institute_id, "institute_id")
  division_id <- validate_optional_id(division_id, "division_id")
  discipline_id <- validate_optional_id(discipline_id, "discipline_id")

  thesis_no <- validate_text_values(thesis_no, "thesis_no", coerce = TRUE)
  title <- validate_text_values(title, "title")
  author <- validate_text_values(author, "author")
  supervisor <- validate_text_values(supervisor, "supervisor")
  abstract <- validate_text_values(abstract, "abstract")
  keyword <- validate_text_values(keyword, "keyword")
  subject <- validate_text_values(subject, "subject")
  university <- validate_text_values(university, "university")
  institute <- validate_text_values(institute, "institute")
  division <- validate_text_values(division, "division")
  discipline <- validate_text_values(discipline, "discipline")
  language <- validate_language_values(language, allow_multiple = TRUE)

  prepare_request <- function() {
    if (!is.null(university) && is.null(university_id)) {
      matched_university <- resolve_lookup_item(
        university,
        lookup_university_item,
        "University"
      )
      if (!is.null(matched_university)) {
        university_id <<- matched_university$id
        university <<- matched_university$name
      }
    }

    if (!is.null(institute) && is.null(institute_id)) {
      matched_institute <- resolve_lookup_item(
        institute,
        lookup_institute_item,
        "Institute"
      )
      if (!is.null(matched_institute)) {
        institute_id <<- matched_institute$id
        institute <<- matched_institute$name
      }
    }

    if (!is.null(division) && is.null(division_id)) {
      matched_division <- resolve_lookup_item(
        division,
        lookup_division_item,
        "Division"
      )
      if (!is.null(matched_division)) {
        division_id <<- matched_division$id
        division <<- matched_division$name
      }
    }

    if (!is.null(discipline) && is.null(discipline_id)) {
      matched_discipline <- resolve_lookup_item(
        discipline,
        lookup_discipline_item,
        "Discipline"
      )
      if (!is.null(matched_discipline)) {
        discipline_id <<- matched_discipline$id
        discipline <<- matched_discipline$name
      }
    }
  }

  year_start <- validate_year(year_start, "year_start")
  year_end <- validate_year(year_end, "year_end")
  if (!is.null(year_start) && !is.null(year_end) && year_start > year_end) {
    cli::cli_abort(
      "{.arg year_start} must be less than or equal to {.arg year_end}"
    )
  }

  has_detailed_criteria <- any(vapply(
    list(
      thesis_no = thesis_no,
      title = title,
      author = author,
      supervisor = supervisor,
      abstract = abstract,
      keyword = keyword,
      subject = subject,
      university = university,
      university_id = university_id,
      institute = institute,
      institute_id = institute_id,
      division = division,
      division_id = division_id,
      discipline = discipline,
      discipline_id = discipline_id,
      year_start = year_start,
      year_end = year_end,
      language = language,
      group = if (!identical(group, "all")) group else NULL,
      thesis_type = if (!identical(thesis_type, "all")) thesis_type else NULL,
      access_type = if (!identical(access_type, "all")) access_type else NULL,
      status = if (!identical(status, "approved")) status else NULL
    ),
    function(value) !is.null(value),
    logical(1)
  ))

  if (!has_detailed_criteria) {
    cli::cli_abort("At least one search criterion or filter must be provided")
  }

  # Check if any parameters have multiple values
  multi_params <- list(
    thesis_no = thesis_no,
    title = title,
    author = author,
    supervisor = supervisor,
    abstract = abstract,
    keyword = keyword,
    subject = subject,
    university = university,
    institute = institute,
    division = division,
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
      subject = subject,
      university = university,
      university_id = university_id,
      institute = institute,
      institute_id = institute_id,
      division = division,
      division_id = division_id,
      discipline = discipline,
      discipline_id = discipline_id,
      group = group,
      thesis_type = thesis_type,
      year_start = year_start,
      year_end = year_end,
      language = language,
      access_type = access_type,
      status = status
    )
  )

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
      subject = subject,
      group = group,
      university = university,
      university_id = university_id,
      institute = institute,
      institute_id = institute_id,
      division = division,
      division_id = division_id,
      discipline = discipline,
      discipline_id = discipline_id,
      thesis_type = thesis_type,
      language = language,
      access_type = access_type,
      status = status
    )
  }

  search_results <- run_search_pipeline(
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

  if (
    nrow(search_results) == 0 &&
      !is.null(university) &&
      has_university_fallback_filter(
        thesis_no = thesis_no,
        title = title,
        author = author,
        supervisor = supervisor,
        abstract = abstract,
        keyword = keyword,
        subject = subject,
        institute = institute,
        institute_id = institute_id,
        division = division,
        division_id = division_id,
        discipline = discipline,
        discipline_id = discipline_id,
        year_start = year_start,
        year_end = year_end,
        language = language,
        group = group,
        thesis_type = thesis_type,
        access_type = access_type,
        status = status
      )
  ) {
    cli::cli_alert_info(
      paste0(
        "YOK returned no rows with the university filter. Retrying without ",
        "that filter and matching the returned rows locally..."
      )
    )

    fallback_cache_key <- build_search_cache_key(
      type = "detailed_university_fallback",
      params = list(
        thesis_no = thesis_no,
        title = title,
        author = author,
        supervisor = supervisor,
        abstract = abstract,
        keyword = keyword,
        subject = subject,
        institute = institute,
        institute_id = institute_id,
        division = division,
        division_id = division_id,
        discipline = discipline,
        discipline_id = discipline_id,
        group = group,
        thesis_type = thesis_type,
        year_start = year_start,
        year_end = year_end,
        language = language,
        access_type = access_type,
        status = status
      )
    )

    fallback_form_builder <- function(range_start, range_end) {
      build_detailed_search_form(
        thesis_no = thesis_no,
        title = title,
        author = author,
        supervisor = supervisor,
        abstract = abstract,
        keyword = keyword,
        university = NULL,
        university_id = NULL,
        institute = institute,
        institute_id = institute_id,
        division = division,
        division_id = division_id,
        subject = subject,
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

    fallback_cache_key_params <- function() {
      list(
        search_type = "detailed_university_fallback",
        thesis_no = thesis_no,
        title = title,
        author = author,
        supervisor = supervisor,
        abstract = abstract,
        keyword = keyword,
        subject = subject,
        group = group,
        institute = institute,
        institute_id = institute_id,
        division = division,
        division_id = division_id,
        discipline = discipline,
        discipline_id = discipline_id,
        thesis_type = thesis_type,
        language = language,
        access_type = access_type,
        status = status
      )
    }

    fallback_results <- run_search_pipeline(
      search_label = "detailed",
      cache_key = fallback_cache_key,
      cache_key_params = fallback_cache_key_params,
      form_builder = fallback_form_builder,
      year_start = year_start,
      year_end = year_end,
      max_search_results = max_search_results,
      requested_all_results = requested_all_results,
      ignore_cache = ignore_cache
    )

    filtered_results <- filter_search_results_by_university(
      fallback_results,
      university
    )

    if (nrow(filtered_results) > 0) {
      cli::cli_alert_success(
        "Returning {.val {nrow(filtered_results)}} locally filtered results"
      )
      search_results <- filtered_results
    }
  }

  search_results
}
