# Vector parameter expansion for tezr package

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
  # Create a grid of all parameter combinations
  param_grid <- expand.grid(
    thesis_no = if (!is.null(thesis_no)) thesis_no else NA_character_,
    title = if (!is.null(title)) title else NA_character_,
    author = if (!is.null(author)) author else NA_character_,
    supervisor = if (!is.null(supervisor)) supervisor else NA_character_,
    abstract = if (!is.null(abstract)) abstract else NA_character_,
    keyword = if (!is.null(keyword)) keyword else NA_character_,
    university = if (!is.null(university)) university else NA_character_,
    institute = if (!is.null(institute)) institute else NA_character_,
    division = if (!is.null(division)) division else NA_character_,
    subject = if (!is.null(subject)) subject else NA_character_,
    discipline = if (!is.null(discipline)) discipline else NA_character_,
    language = if (!is.null(language)) language else NA_character_,
    thesis_type = thesis_type,
    access_type = access_type,
    group = group,
    stringsAsFactors = FALSE
  )

  # Remove rows that are all NA (shouldn't happen but just in case)
  param_grid <- param_grid[rowSums(!is.na(param_grid)) > 0, , drop = FALSE]

  n_searches <- nrow(param_grid)
  cli::cli_alert_info(
    "Expanding search to {.val {n_searches}} combination{?s} based on multiple values"
  )

  all_search_results <- list()

  cli::cli_progress_bar(
    "Running searches",
    total = n_searches,
    clear = FALSE
  )

  na_to_null <- function(x) if (is.na(x)) NULL else x

  for (i in seq_len(n_searches)) {
    cli::cli_progress_update()

    tryCatch(
      {
        search_results <- search_detailed(
          thesis_no = na_to_null(param_grid$thesis_no[i]),
          title = na_to_null(param_grid$title[i]),
          author = na_to_null(param_grid$author[i]),
          supervisor = na_to_null(param_grid$supervisor[i]),
          abstract = na_to_null(param_grid$abstract[i]),
          keyword = na_to_null(param_grid$keyword[i]),
          university = na_to_null(param_grid$university[i]),
          university_id = university_id,
          institute = na_to_null(param_grid$institute[i]),
          institute_id = institute_id,
          division = na_to_null(param_grid$division[i]),
          division_id = division_id,
          subject = na_to_null(param_grid$subject[i]),
          discipline = na_to_null(param_grid$discipline[i]),
          discipline_id = discipline_id,
          thesis_type = param_grid$thesis_type[i],
          year_start = year_start,
          year_end = year_end,
          language = na_to_null(param_grid$language[i]),
          access_type = param_grid$access_type[i],
          group = param_grid$group[i],
          status = status,
          max_search_results = max_search_results,
          ignore_cache = ignore_cache
        )

        if (nrow(search_results) > 0) {
          all_search_results[[length(all_search_results) + 1]] <- search_results
        }

        # Rate limiting between searches
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

  if (length(all_search_results) == 0) {
    return(empty_results_tibble())
  }

  # Combine and deduplicate
  combined <- dplyr::bind_rows(all_search_results) |>
    dplyr::distinct(.data$thesis_no, .keep_all = TRUE)

  combined_count <- nrow(combined)

  # Trim to max_search_results
  if (limit_combined_results && combined_count > max_search_results) {
    cli::cli_alert_warning(
      paste0(
        "Combined {.val {combined_count}} unique results from ",
        "{.val {n_searches}} searches; returning {.val {max_search_results}} ",
        "because {.arg max_search_results} is set. Set ",
        "{.arg max_search_results = Inf} to return all combined results."
      )
    )
    combined <- combined[seq_len(max_search_results), ]
  } else {
    cli::cli_alert_success(
      "Combined {.val {combined_count}} unique results from {.val {n_searches}} searches"
    )
  }

  return(combined)
}
