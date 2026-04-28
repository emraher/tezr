#' Base URL
#' @noRd
base_url <- "https://tez.yok.gov.tr/UlusalTezMerkezi/"

#' Endpoints
#' @noRd
endpoints <- list(
  home = "giris.jsp",
  search = "SearchTez",
  results = "tezSorguSonucYeni.jsp",
  detail = "tezDetay.jsp"
)

#' Thesis Environment
#' @noRd
tezr_env <- new.env(parent = emptyenv())

#' Return browser-like HTTP headers used for all requests
#' @noRd
default_request_headers <- function() {
  c(
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36", # nolint: line_length_linter
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language" = "tr-TR,tr;q=0.9,en-US;q=0.8,en;q=0.7",
    "Content-Type" = "application/x-www-form-urlencoded"
  )
}

#' Create an HTTP session with proper headers
#'
#' @param ssl_verify Logical. Whether to verify SSL certificates. Default FALSE
#'   due to certificate issues on the server.
#' @param apply_rate_limit Logical. Apply built-in request throttling before
#'   creating the request? Default TRUE.
#' @return An httr2 request object
#' @noRd
create_session <- function(ssl_verify = FALSE, apply_rate_limit = TRUE) {
  if (isTRUE(apply_rate_limit)) {
    rate_limit()
  }

  req <- httr2::request(base_url) |>
    httr2::req_options(ssl_verifypeer = ssl_verify) |>
    httr2::req_headers(!!!default_request_headers()) |>
    httr2::req_retry(max_tries = 3)

  # Add cookies if we have a session
  if (!is.null(tezr_env$cookies)) {
    req <- httr2::req_headers(req, "Cookie" = tezr_env$cookies)
  }

  return(req)
}

#' Visit the homepage to obtain session cookies
#' @noRd
init_session <- function(ssl_verify = FALSE) {
  init_cache()

  req <- httr2::request(base_url) |>
    httr2::req_url_path_append(endpoints$home) |>
    httr2::req_options(ssl_verifypeer = ssl_verify) |>
    httr2::req_headers(!!!default_request_headers())

  resp <- httr2::req_perform(req)

  cookies <- httr2::resp_header(resp, "set-cookie")
  if (!is.null(cookies)) {
    tezr_env$cookies <- stringr::str_extract(cookies, "^[^;]+")
  }

  tezr_env$request_count <- 0L
  tezr_env$session_start <- Sys.time()

  invisible(TRUE)
}

#' Refresh session if stale
#'
#' Re-initializes the session after 50 requests or 20 minutes, whichever
#' comes first. Called during long-running batch operations.
#' @noRd
refresh_session_if_needed <- function() {
  count <- tezr_env$request_count %||% 0L
  start <- tezr_env$session_start

  stale_by_count <- count >= 50L
  stale_by_time <- !is.null(start) &&
    as.numeric(difftime(Sys.time(), start, units = "mins")) >= 20

  if (stale_by_count || stale_by_time) {
    reason <- if (stale_by_count && stale_by_time) {
      "request count and session age thresholds reached"
    } else if (stale_by_count) {
      "request count threshold reached"
    } else {
      "session age threshold reached"
    }

    cli::cli_alert_info(paste0("Refreshing session (", reason, ")..."))
    init_session()
  }
}

#' Increment the per-session request counter by one
#' @noRd
increment_request_count <- function() {
  tezr_env$request_count <- (tezr_env$request_count %||% 0L) + 1L
}

#' Check whether a session with valid cookies exists
#' @noRd
has_session <- function() {
  return(!is.null(tezr_env$cookies))
}

#' Enforce minimum delay between consecutive HTTP requests
#' @noRd
rate_limit <- function(delay = 2) {
  last_request <- tezr_env$last_request

  if (!is.null(last_request)) {
    elapsed <- as.numeric(difftime(Sys.time(), last_request, units = "secs"))
    if (elapsed < delay) {
      Sys.sleep(delay - elapsed)
    }
  }

  tezr_env$last_request <- Sys.time()
  invisible(NULL)
}
