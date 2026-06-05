# Vector parameter expansion for tezr package

#' Expand NULL vector parameters to the grid sentinel
#' @noRd
expand_param <- function(value) {
  if (is.null(value)) {
    return(NA_character_)
  }

  value
}

#' Convert expansion sentinel values back to NULL
#' @noRd
na_to_null <- function(value) {
  if (is.na(value)) {
    return(NULL)
  }

  value
}

#' Create a grid of detailed-search parameter combinations
#' @noRd
build_detailed_search_grid <- function(
  thesis_no = NULL,
  title = NULL,
  author = NULL,
  supervisor = NULL,
  abstract = NULL,
  keyword = NULL,
  university = NULL,
  institute = NULL,
  division = NULL,
  subject = NULL,
  discipline = NULL,
  language = NULL,
  thesis_type = "all",
  access_type = "all",
  group = "all"
) {
  param_grid <- expand.grid(
    thesis_no = expand_param(thesis_no),
    title = expand_param(title),
    author = expand_param(author),
    supervisor = expand_param(supervisor),
    abstract = expand_param(abstract),
    keyword = expand_param(keyword),
    university = expand_param(university),
    institute = expand_param(institute),
    division = expand_param(division),
    subject = expand_param(subject),
    discipline = expand_param(discipline),
    language = expand_param(language),
    thesis_type = thesis_type,
    access_type = access_type,
    group = group,
    stringsAsFactors = FALSE
  )

  param_grid[rowSums(!is.na(param_grid)) > 0, , drop = FALSE]
}

#' Run one detailed search combination from the expanded parameter grid
#' @noRd
search_detailed_grid_row <- function(
  param_grid,
  row_index,
  university_id,
  institute_id,
  division_id,
  discipline_id,
  year_start,
  year_end,
  status,
  max_search_results,
  ignore_cache
) {
  search_detailed(
    thesis_no = na_to_null(param_grid$thesis_no[row_index]),
    title = na_to_null(param_grid$title[row_index]),
    author = na_to_null(param_grid$author[row_index]),
    supervisor = na_to_null(param_grid$supervisor[row_index]),
    abstract = na_to_null(param_grid$abstract[row_index]),
    keyword = na_to_null(param_grid$keyword[row_index]),
    university = na_to_null(param_grid$university[row_index]),
    university_id = university_id,
    institute = na_to_null(param_grid$institute[row_index]),
    institute_id = institute_id,
    division = na_to_null(param_grid$division[row_index]),
    division_id = division_id,
    subject = na_to_null(param_grid$subject[row_index]),
    discipline = na_to_null(param_grid$discipline[row_index]),
    discipline_id = discipline_id,
    thesis_type = param_grid$thesis_type[row_index],
    year_start = year_start,
    year_end = year_end,
    language = na_to_null(param_grid$language[row_index]),
    access_type = param_grid$access_type[row_index],
    group = param_grid$group[row_index],
    status = status,
    max_search_results = max_search_results,
    ignore_cache = ignore_cache
  )
}

#' Collect detailed search results across an expanded parameter grid
#' @noRd
collect_expanded_search_results <- function(
  param_grid,
  university_id,
  institute_id,
  division_id,
  discipline_id,
  year_start,
  year_end,
  status,
  max_search_results,
  ignore_cache
) {
  n_searches <- nrow(param_grid)
  all_search_results <- list()

  cli::cli_progress_bar("Running searches", total = n_searches, clear = FALSE)

  for (i in seq_len(n_searches)) {
    cli::cli_progress_update()

    tryCatch(
      {
        search_results <- search_detailed_grid_row(
          param_grid,
          i,
          university_id,
          institute_id,
          division_id,
          discipline_id,
          year_start,
          year_end,
          status,
          max_search_results,
          ignore_cache
        )

        if (nrow(search_results) > 0) {
          all_search_results[[length(all_search_results) + 1]] <- search_results
        }

        if (i < n_searches) {
          Sys.sleep(2)
        }
      },
      error = function(e) {
        cli::cli_alert_warning("Search {i} failed: {e$message}")
      }
    )
  }

  cli::cli_progress_done()
  return(all_search_results)
}

#' Combine and optionally trim expanded detailed-search results
#' @noRd
finalize_expanded_search_results <- function(
  all_search_results,
  n_searches,
  max_search_results,
  limit_combined_results
) {
  if (length(all_search_results) == 0) {
    return(empty_results_tibble())
  }

  combined <- dplyr::bind_rows(all_search_results) |>
    dplyr::distinct(.data$thesis_no, .keep_all = TRUE)

  combined_count <- nrow(combined)

  if (limit_combined_results && combined_count > max_search_results) {
    cli::cli_alert_warning(
      paste0(
        "Combined {.val {combined_count}} unique results from ",
        "{.val {n_searches}} searches; returning {.val {max_search_results}} ",
        "because {.arg max_search_results} is set. Set ",
        "{.arg max_search_results = Inf} to return all combined results."
      )
    )
    return(combined[seq_len(max_search_results), ])
  }

  tezr_success(paste0(
    "Combined {.val {combined_count}} unique results from ",
    "{.val {n_searches}} searches"
  ))
  return(combined)
}

#' Expand and search with multiple values for detailed search
#'
#' Internal helper that runs search_detailed multiple times when
#' any parameter has multiple values, then combines and deduplicates results.
#'
#' @inheritParams search_detailed
#' @return Combined tibble of results
#' @noRd
expand_and_search_detailed <- function(
  thesis_no = NULL,
  title = NULL,
  author = NULL,
  supervisor = NULL,
  abstract = NULL,
  keyword = NULL,
  university = NULL,
  university_id = NULL,
  institute = NULL,
  institute_id = NULL,
  division = NULL,
  division_id = NULL,
  subject = NULL,
  discipline = NULL,
  discipline_id = NULL,
  thesis_type = "all",
  year_start = NULL,
  year_end = NULL,
  language = NULL,
  access_type = "all",
  group = "all",
  status = "approved",
  max_search_results = 2000,
  limit_combined_results = TRUE,
  ignore_cache = FALSE
) {
  param_grid <- build_detailed_search_grid(
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

  n_searches <- nrow(param_grid)
  tezr_inform(paste0(
    "Expanding search to {.val {n_searches}} combination{?s} ",
    "based on multiple values"
  ))

  all_search_results <- collect_expanded_search_results(
    param_grid,
    university_id,
    institute_id,
    division_id,
    discipline_id,
    year_start,
    year_end,
    status,
    max_search_results,
    ignore_cache
  )

  finalize_expanded_search_results(
    all_search_results,
    n_searches,
    max_search_results,
    limit_combined_results
  )
}
