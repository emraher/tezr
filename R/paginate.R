# Year-range pagination for tezr package

#' Fetch results by adaptive year range splitting
#'
#' When total results exceed the server's 2000 limit, this function
#' adaptively subdivides the year range to handle non-uniform distributions.
#'
#' @param total_count Total results reported by server
#' @param current_search_results Results already fetched
#' @param max_search_results Maximum results to fetch
#' @param year_start Start year
#' @param year_end End year
#' @param form_builder Function that builds form data for a year range.
#' @param cache_key_params List of parameters to include in cache key.
#' @param ignore_cache Logical. If `TRUE`, bypass range-cache reads/writes.
#' @return Combined tibble of results
#' @noRd
fetch_by_year_ranges <- function(
  total_count,
  current_search_results,
  max_search_results,
  year_start,
  year_end,
  form_builder,
  cache_key_params,
  ignore_cache = FALSE
) {
  tezr_inform(paste0(
    "Auto-pagination: target {.val {total_count}} results across ",
    "{.val {year_start}}-{.val {year_end}}"
  ))

  all_search_results <- fetch_year_range_iterative(
    total_count = total_count,
    year_start = year_start,
    year_end = year_end,
    form_builder = form_builder,
    cache_key_params = cache_key_params,
    max_search_results = max_search_results,
    ignore_cache = ignore_cache
  )
  single_year_overflow_years <- attr(
    all_search_results,
    "single_year_overflow_years",
    exact = TRUE
  )

  if (length(all_search_results) == 0) {
    return(current_search_results)
  }

  combined <- dplyr::bind_rows(all_search_results) |>
    dplyr::distinct(.data$thesis_no, .keep_all = TRUE)

  tezr_success(
    "Pagination complete: {.val {nrow(combined)}} unique results"
  )

  attr(combined, "single_year_overflow_years") <- single_year_overflow_years
  return(combined)
}

#' Iterative helper for multi-way year range splitting
#'
#' Uses a work queue instead of recursion. Computes the minimum number of
#' chunks via `ceiling(total_count / 2000)`, splits the year range into
#' that many pieces using density-weighted split points, then fetches each.
#' If a chunk still exceeds 2000, it is re-split and re-queued.
#'
#' @param total_count Total results reported by server for the full range.
#' @param year_start Start year (inclusive).
#' @param year_end End year (inclusive).
#' @param form_builder Function(range_start, range_end) returning form data.
#' @param cache_key_params List of parameters for cache key construction.
#' @param max_search_results Stop collecting after this many rows.
#' @param ignore_cache Logical. If `TRUE`, bypass range-cache reads/writes.
#' @return List of tibbles (one per successfully fetched chunk).
#' @noRd
fetch_year_range_iterative <- function(
  total_count,
  year_start,
  year_end,
  form_builder,
  cache_key_params,
  max_search_results,
  ignore_cache = FALSE
) {
  if (is.null(cache_key_params)) {
    cache_key_params <- list()
  }

  density_state <- initialize_density_state()
  ranges <- initial_year_ranges(
    year_start,
    year_end,
    total_count,
    density_state
  )
  collected <- list()
  collected_count <- 0L
  single_year_overflow_years <- integer(0)

  while (length(ranges) > 0 && collected_count < max_search_results) {
    range <- ranges[[1]]
    ranges <- ranges[-1]

    result <- process_year_range_iteration(
      range,
      density_state,
      form_builder,
      cache_key_params,
      ignore_cache
    )

    if (result$resplit) {
      ranges <- c(result$ranges, ranges)
      next
    }

    single_year_overflow_years <- c(
      single_year_overflow_years,
      result$overflow_years
    )
    density_state <- result$density_state
    collected[[length(collected) + 1]] <- result$search_results
    collected_count <- collected_count + result$count
  }

  attr(collected, "single_year_overflow_years") <- sort(unique(
    single_year_overflow_years
  ))
  return(collected)
}

#' Build the initial year-range queue
#' @noRd
initial_year_ranges <- function(
  year_start,
  year_end,
  total_count,
  density_state
) {
  build_ranges_from_split(
    year_start = year_start,
    year_end = year_end,
    total_count = total_count,
    era_weights = effective_era_weights(density_state)
  )
}

#' Process one year-range queue item
#' @noRd
process_year_range_iteration <- function(
  range,
  density_state,
  form_builder,
  cache_key_params,
  ignore_cache
) {
  r_start <- range$start
  r_end <- range$end
  range_result <- fetch_year_range_result(
    r_start,
    r_end,
    form_builder,
    cache_key_params,
    ignore_cache
  )

  if (
    needs_year_range_resplit(
      r_start,
      r_end,
      range_result$total_count,
      range_result$count
    )
  ) {
    return(resplit_year_range_result(
      r_start,
      r_end,
      range_result,
      density_state
    ))
  }

  complete_year_range_result(r_start, r_end, range_result, density_state)
}

#' Return a re-split year-range iteration result
#' @noRd
resplit_year_range_result <- function(
  r_start,
  r_end,
  range_result,
  density_state
) {
  list(
    resplit = TRUE,
    ranges = split_overflowing_year_range(
      r_start,
      r_end,
      range_result$total_count,
      range_result$count,
      density_state
    )
  )
}

#' Return a completed year-range iteration result
#' @noRd
complete_year_range_result <- function(
  r_start,
  r_end,
  range_result,
  density_state
) {
  overflow_years <- warn_year_range_incomplete(
    r_start,
    r_end,
    range_result$total_count,
    range_result$count
  )

  list(
    resplit = FALSE,
    search_results = range_result$search_results,
    count = range_result$count,
    overflow_years = overflow_years,
    density_state = update_density_state(
      state = density_state,
      year_start = r_start,
      year_end = r_end,
      range_total = range_result$total_count,
      range_count = range_result$count
    )
  )
}

#' Fetch one year-range chunk from cache or the server
#' @noRd
fetch_year_range_result <- function(
  r_start,
  r_end,
  form_builder,
  cache_key_params,
  ignore_cache
) {
  cache_key <- build_year_range_cache_key(r_start, r_end, cache_key_params)
  cached_range <- get_cached_year_range(cache_key, ignore_cache)

  if (!is.null(cached_range)) {
    return(normalize_year_range_result(cached_range))
  }

  range_data <- request_year_range_result(r_start, r_end, form_builder)
  if (!ignore_cache) {
    set_cached(tezr_env$range_cache, cache_key, range_data)
  }

  normalize_year_range_result(range_data)
}

#' Build a cache key for one year range
#' @noRd
build_year_range_cache_key <- function(r_start, r_end, cache_key_params) {
  build_search_cache_key(
    type = "year_range",
    params = c(
      list(year_start = r_start, year_end = r_end),
      cache_key_params
    )
  )
}

#' Return cached year-range data unless cache bypass is requested
#' @noRd
get_cached_year_range <- function(cache_key, ignore_cache) {
  if (ignore_cache) {
    return(NULL)
  }

  get_cached(
    tezr_env$range_cache,
    cache_key,
    tezr_env$search_ttl
  )
}

#' Fetch one year range from the search endpoint with warning fallback
#' @noRd
request_year_range_result <- function(r_start, r_end, form_builder) {
  tryCatch(
    {
      search_data <- perform_search_request(form_builder(r_start, r_end))
      list(
        search_results = search_data$search_results,
        total_count = search_data$total_count
      )
    },
    error = function(e) {
      cli::cli_alert_warning(
        "Failed to fetch years {r_start}-{r_end}: {e$message}"
      )
      list(search_results = empty_results_tibble(), total_count = 0L)
    }
  )
}

#' Add a row count to year-range search data
#' @noRd
normalize_year_range_result <- function(range_data) {
  range_data$count <- nrow(range_data$search_results)
  range_data
}

#' Return whether a capped year-range result should be split again
#' @noRd
needs_year_range_resplit <- function(r_start, r_end, range_total, range_count) {
  range_total > range_count &&
    r_end > r_start &&
    range_count >= 1999
}

#' Re-split an overflowing year range and emit the existing progress message
#' @noRd
split_overflowing_year_range <- function(
  r_start,
  r_end,
  range_total,
  range_count,
  density_state
) {
  tezr_inform(
    paste0(
      "Range {r_start}-{r_end}: total {.val {range_total}}, ",
      "retrieved {.val {range_count}} (limit). Re-splitting..."
    )
  )

  build_ranges_from_split(
    year_start = r_start,
    year_end = r_end,
    total_count = range_total,
    era_weights = effective_era_weights(density_state)
  )
}

#' Warn on incomplete year ranges and return unsplittable overflow years
#' @noRd
warn_year_range_incomplete <- function(
  r_start,
  r_end,
  range_total,
  range_count
) {
  tezr_inform(
    "Range {r_start}-{r_end}: {.val {range_count}} results retrieved."
  )

  if (range_total > range_count && range_count >= 1999) {
    warn_capped_year_range(r_start, r_end, range_total, range_count)
    if (r_start == r_end) {
      return(as.integer(r_start))
    }
  } else if (range_total > range_count) {
    warn_inconsistent_year_range(r_start, r_end, range_total, range_count)
  }

  integer(0)
}

#' Warn when a year range hits the server cap
#' @noRd
warn_capped_year_range <- function(r_start, r_end, range_total, range_count) {
  if (r_start == r_end) {
    cli::cli_alert_warning(
      paste0(
        "Range {r_start}-{r_end} has {.val {range_total}} ",
        "results but only ",
        "{.val {range_count}} returned (server limit). This is a ",
        "single-year range and cannot split further. Some results were ",
        "truncated. Add more filters to narrow the search."
      )
    )
    return(invisible(NULL))
  }

  cli::cli_alert_warning(
    paste0(
      "Range {r_start}-{r_end} has {.val {range_total}} ",
      "results but only ",
      "{.val {range_count}} returned (server limit). Some results were ",
      "truncated. Add more filters to narrow the search."
    )
  )
  invisible(NULL)
}

#' Warn when the server reports more rows than the parser saw
#' @noRd
warn_inconsistent_year_range <- function(
  r_start,
  r_end,
  range_total,
  range_count
) {
  cli::cli_alert_warning(
    paste0(
      "Range {r_start}-{r_end} reported {.val {range_total}} results but ",
      "parsed {.val {range_count}}. Some results may be missing due to ",
      "response inconsistencies."
    )
  )

  invisible(NULL)
}

#' Build year ranges from density-weighted split points
#'
#' @param year_start Start year (inclusive).
#' @param year_end End year (inclusive).
#' @param total_count Expected total results in this range.
#' @param era_weights Named numeric vector with era weights.
#' @return Target chunk size used to estimate number of split chunks.
#' @noRd
calculate_target_chunk_size <- function(
  year_start,
  year_end,
  total_count,
  era_weights = default_era_weights()
) {
  if (year_start > year_end) {
    return(1500L)
  }

  years <- seq.int(year_start, year_end)
  range_weights <- year_density_weight(years, era_weights = era_weights)
  average_weight <- mean(range_weights)
  baseline_weight <- era_weights[["y2000_2010"]]

  density_ratio <- average_weight / baseline_weight
  scaled_chunk_size <- 1500 / sqrt(density_ratio)

  # Very large totals benefit from smaller chunks to reduce capped probes
  if (!is.null(total_count) && is.finite(total_count) && total_count > 10000) {
    scaled_chunk_size <- scaled_chunk_size * 0.9
  }

  target_chunk_size <- as.integer(round(scaled_chunk_size))
  target_chunk_size <- max(900L, min(1700L, target_chunk_size))

  return(target_chunk_size)
}

#' Build year ranges from density-weighted split points
#'
#' @param year_start Start year (inclusive).
#' @param year_end End year (inclusive).
#' @param total_count Expected total results in this range.
#' @param era_weights Named numeric vector with era weights.
#' @return List of list(start, end) pairs.
#' @noRd
build_ranges_from_split <- function(
  year_start,
  year_end,
  total_count,
  era_weights = default_era_weights()
) {
  target_chunk_size <- calculate_target_chunk_size(
    year_start = year_start,
    year_end = year_end,
    total_count = total_count,
    era_weights = era_weights
  )
  n_chunks <- max(ceiling(total_count / target_chunk_size), 2L)

  split_pts <- calculate_split_points(
    year_start,
    year_end,
    n_chunks,
    era_weights
  )

  # Build ranges that cover [year_start, split1], [split1+1, split2],
  # ..., [splitN+1, year_end].
  boundaries <- c(year_start - 1L, split_pts, year_end)
  ranges <- vector("list", length(boundaries) - 1)
  for (i in seq_along(ranges)) {
    ranges[[i]] <- list(start = boundaries[i] + 1L, end = boundaries[i + 1])
  }

  return(ranges)
}

#' Density weight for a single year
#'
#' Returns a relative weight reflecting how many theses are typically
#' published in a given year. Pre-2000 is sparse, 2000-2010 moderate,
#' 2010+ dense.
#'
#' @param year Integer year.
#' @param era_weights Named numeric vector with era weights.
#' @return Numeric weight.
#' @noRd
year_density_weight <- function(year, era_weights = default_era_weights()) {
  dplyr::case_when(
    year < 2000 ~ era_weights[["pre2000"]],
    year <= 2010 ~ era_weights[["y2000_2010"]],
    .default = era_weights[["post2010"]]
  )
}

#' Calculate multiple split points for multi-way year range splitting
#'
#' Distributes `n_chunks - 1` split points across a year range based on
#' density heuristics so that each chunk carries roughly equal thesis load.
#'
#' @param year_start Start year (inclusive).
#' @param year_end End year (inclusive).
#' @param n_chunks Number of chunks to produce (must be >= 2).
#' @param era_weights Named numeric vector with era weights.
#' @return Integer vector of `n_chunks - 1` split years. Each chunk is
#'   `[prev_split+1, split]` with the first starting at `year_start` and
#'   the last ending at `year_end`.
#' @noRd
calculate_split_points <- function(
  year_start,
  year_end,
  n_chunks,
  era_weights = default_era_weights()
) {
  if (n_chunks < 2) {
    return(integer(0))
  }

  years <- seq.int(year_start, year_end)
  weights <- year_density_weight(years, era_weights = era_weights)
  cum_weights <- cumsum(weights)
  total_weight <- cum_weights[length(cum_weights)]

  split_years <- integer(n_chunks - 1)
  for (i in seq_len(n_chunks - 1)) {
    target <- total_weight * i / n_chunks
    idx <- which(cum_weights >= target)[1]
    split_years[i] <- years[idx]
  }

  # Ensure split points are strictly increasing and within bounds
  split_years <- unique(split_years)
  split_years <- split_years[split_years >= year_start & split_years < year_end]

  # If deduplication reduced the count, fill in evenly

  if (length(split_years) < n_chunks - 1) {
    span <- year_end - year_start
    needed <- n_chunks - 1
    split_years <- year_start + round(seq_len(needed) * span / n_chunks)
    split_years <- unique(split_years)
    split_years <- split_years[
      split_years >= year_start & split_years < year_end
    ]
  }

  return(as.integer(split_years))
}

#' Default era weights for year-density splitting
#' @noRd
default_era_weights <- function() {
  return(c(pre2000 = 1, y2000_2010 = 3, post2010 = 8))
}

#' Initialize adaptive density state for the current pagination run
#' @noRd
initialize_density_state <- function() {
  prior <- default_era_weights()
  return(list(
    prior = prior,
    obs_sum = stats::setNames(rep(0, length(prior)), names(prior)),
    obs_years = stats::setNames(rep(0L, length(prior)), names(prior))
  ))
}

#' Count overlap years per era for a year range
#' @noRd
overlap_years_by_era <- function(year_start, year_end) {
  if (year_start > year_end) {
    return(c(pre2000 = 0L, y2000_2010 = 0L, post2010 = 0L))
  }

  years <- seq.int(year_start, year_end)
  return(c(
    pre2000 = sum(years < 2000),
    y2000_2010 = sum(years >= 2000 & years <= 2010),
    post2010 = sum(years >= 2011)
  ))
}

#' Update adaptive density state with observed uncapped range results
#' @noRd
update_density_state <- function(
  state,
  year_start,
  year_end,
  range_total,
  range_count
) {
  # Capped ranges are lower bounds and should not influence dynamic weights.
  if (range_total > range_count && range_count >= 1999) {
    return(state)
  }

  overlaps <- overlap_years_by_era(year_start, year_end)
  span <- sum(overlaps)
  if (span <= 0) {
    return(state)
  }

  observed_total <- min(range_total, range_count)
  if (observed_total <= 0) {
    return(state)
  }

  per_year_rate <- observed_total / span
  state$obs_sum <- state$obs_sum + per_year_rate * overlaps
  state$obs_years <- state$obs_years + overlaps
  return(state)
}

#' Derive effective era weights by combining prior and observed densities
#' @noRd
effective_era_weights <- function(state) {
  weights <- state$prior
  observed <- state$obs_years > 0
  if (!any(observed)) {
    return(weights)
  }

  obs_rates <- state$obs_sum[observed] / state$obs_years[observed]
  min_rate <- suppressWarnings(min(obs_rates[obs_rates > 0], na.rm = TRUE))
  if (!is.finite(min_rate) || min_rate <= 0) {
    return(weights)
  }

  obs_scaled <- obs_rates / min_rate
  obs_scaled <- pmax(0.5, pmin(obs_scaled, 30))
  weights[names(obs_scaled)] <- as.numeric(obs_scaled)
  return(weights)
}
