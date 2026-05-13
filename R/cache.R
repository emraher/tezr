#' Configure cache behavior
#'
#' Controls whether search results and thesis details are cached in memory
#' during the R session. Caching can significantly speed up repeated queries.
#'
#' @param enable Logical. Enable caching? Default TRUE.
#' @param search_ttl Numeric. Search result TTL (time-to-live) in seconds.
#'   NULL means cache lives for entire session. Default is 3600 (1 hour).
#' @param detail_ttl Numeric. Detail page TTL in seconds.
#'   NULL means cache lives for entire session. Default is NULL (session).
#'
#' @return Invisible NULL
#' @export
#'
#' @examples
#' # Disable caching entirely
#' cache_config(enable = FALSE)
#'
#' # Enable with 30-minute TTL for searches
#' cache_config(enable = TRUE, search_ttl = 1800)
#'
#' # Cache details forever (until session ends)
#' cache_config(detail_ttl = NULL)
cache_config <- function(
  enable = TRUE,
  search_ttl = 3600,
  detail_ttl = NULL
) {
  enable <- validate_cache_enable(enable)
  search_ttl <- validate_cache_ttl(search_ttl, "search_ttl")
  detail_ttl <- validate_cache_ttl(detail_ttl, "detail_ttl")

  tezr_env$cache_enabled <- enable
  tezr_env$search_ttl <- search_ttl
  tezr_env$detail_ttl <- detail_ttl
  return(invisible(NULL))
}

#' Validate cache enabled flag
#' @noRd
validate_cache_enable <- function(enable) {
  if (
    !is.logical(enable) ||
      length(enable) != 1L ||
      is.na(enable)
  ) {
    cli::cli_abort("{.arg enable} must be TRUE or FALSE")
  }

  enable
}

#' Validate a cache TTL value
#' @noRd
validate_cache_ttl <- function(ttl, arg_name) {
  if (is.null(ttl)) {
    return(NULL)
  }

  if (
    !is.numeric(ttl) ||
      length(ttl) != 1L ||
      is.na(ttl) ||
      ttl < 0
  ) {
    cli::cli_abort(
      "{.arg {arg_name}} must be a single non-negative number or NULL"
    )
  }

  ttl
}

#' Clear the cache
#'
#' Removes cached search results, thesis details, and/or lookup data from memory.
#'
#' @param what Character. What to clear: "all", "searches", "details", or "lookups".
#'   Default is "all".
#'
#' @return Invisible NULL
#' @export
#'
#' @examples
#' # Clear everything
#' cache_clear()
#'
#' # Clear only search results
#' cache_clear("searches")
#'
#' # Clear only thesis details
#' cache_clear("details")
cache_clear <- function(what = c("all", "searches", "details", "lookups")) {
  what <- rlang::arg_match(what)

  if (what %in% c("all", "searches")) {
    tezr_env$search_cache <- new.env(parent = emptyenv())
    tezr_env$range_cache <- new.env(parent = emptyenv())
    cli::cli_alert_success("Search cache cleared")
  }

  if (what %in% c("all", "details")) {
    tezr_env$detail_cache <- new.env(parent = emptyenv())
    cli::cli_alert_success("Detail cache cleared")
  }

  if (what %in% c("all", "lookups")) {
    # Clear the lookup_cache from lookup.R
    if (exists("lookup_cache", envir = asNamespace("tezr"))) {
      lookup_env <- get("lookup_cache", envir = asNamespace("tezr"))
      rm(list = ls(lookup_env), envir = lookup_env)
      cli::cli_alert_success("Lookup cache cleared")
    }
  }

  invisible(NULL)
}

#' Get cache statistics
#'
#' Returns information about the current cache state, including number of
#' cached items and configuration settings.
#'
#' @return A list with cache statistics:
#'   \itemize{
#'     \item enabled - Whether caching is enabled
#'     \item search_count - Number of cached search results
#'     \item range_count - Number of cached year-range searches
#'     \item detail_count - Number of cached thesis details
#'     \item search_ttl - Search cache TTL in seconds (NULL = session)
#'     \item detail_ttl - Detail cache TTL in seconds (NULL = session)
#'   }
#' @export
#'
#' @examples
#' cache_info()
cache_info <- function() {
  search_count <- if (!is.null(tezr_env$search_cache)) {
    length(ls(tezr_env$search_cache))
  } else {
    0L
  }

  range_count <- if (!is.null(tezr_env$range_cache)) {
    length(ls(tezr_env$range_cache))
  } else {
    0L
  }

  detail_count <- if (!is.null(tezr_env$detail_cache)) {
    length(ls(tezr_env$detail_cache))
  } else {
    0L
  }

  return(list(
    enabled = cache_enabled(),
    search_count = search_count,
    range_count = range_count,
    detail_count = detail_count,
    search_ttl = tezr_env$search_ttl,
    detail_ttl = tezr_env$detail_ttl
  ))
}

#' Check whether caching is enabled
#' @noRd
cache_enabled <- function() {
  return(isTRUE(tezr_env$cache_enabled %|na|% TRUE))
}

#' Initialize cache environments if they do not exist yet
#' @noRd
init_cache <- function() {
  if (is.null(tezr_env$search_cache)) {
    tezr_env$search_cache <- new.env(parent = emptyenv())
  }
  if (is.null(tezr_env$range_cache)) {
    tezr_env$range_cache <- new.env(parent = emptyenv())
  }
  if (is.null(tezr_env$detail_cache)) {
    tezr_env$detail_cache <- new.env(parent = emptyenv())
  }
  if (is.null(tezr_env$cache_enabled)) {
    tezr_env$cache_enabled <- TRUE
  }
  if (is.null(tezr_env$search_ttl)) {
    tezr_env$search_ttl <- 3600
  }
  return(invisible(NULL))
}

#' Hash sorted parameters into a cache key for search results
#' @noRd
make_search_key <- function(...) {
  params <- list(...)
  params <- params[order(names(params))]
  param_str <- paste(names(params), params, sep = "=", collapse = "|")
  return(rlang::hash(param_str))
}

#' Build search cache key
#' @noRd
build_search_cache_key <- function(type, params) {
  if (is.null(params)) {
    params <- list()
  }
  do.call(make_search_key, c(list(type = type), params))
}

#' Build a cache key for a thesis detail page
#' @noRd
make_detail_key <- function(detail_id, thesis_no) {
  return(paste0("d_", detail_id, "_", thesis_no))
}

#' Retrieve a value from cache, returning NULL if missing or expired
#' @noRd
get_cached <- function(cache_env, key, ttl = NULL) {
  if (!cache_enabled() || is.null(cache_env)) {
    return(NULL)
  }

  cached <- cache_env[[key]]
  if (is.null(cached)) {
    return(NULL)
  }

  if (!is.null(ttl) && !is.null(cached$timestamp)) {
    age <- as.numeric(difftime(Sys.time(), cached$timestamp, units = "secs"))
    if (age > ttl) {
      rm(list = key, envir = cache_env)
      return(NULL)
    }
  }

  return(cached$value)
}

#' Store a value in cache with a timestamp
#' @noRd
set_cached <- function(cache_env, key, value) {
  if (!cache_enabled() || is.null(cache_env)) {
    return(invisible(NULL))
  }

  cache_env[[key]] <- list(
    value = value,
    timestamp = Sys.time()
  )
  invisible(NULL)
}
