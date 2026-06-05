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

#' Return the package-identifying user agent
#' @noRd
package_user_agent <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("tezr")),
    error = function(...) "dev"
  )
  ci_context <- if (identical(Sys.getenv("GITHUB_ACTIONS"), "true")) {
    ", GitHub Actions"
  } else {
    ""
  }

  paste0(
    "tezr/",
    version,
    " (https://github.com/emraher/tezr, mailto:eer@eremrah.com",
    ci_context,
    ")"
  )
}

#' Validate a request user agent value
#' @noRd
validate_user_agent <- function(user_agent) {
  if (
    !is.character(user_agent) ||
      length(user_agent) != 1L ||
      is.na(user_agent) ||
      nchar(clean_text(user_agent)) == 0L
  ) {
    cli::cli_abort(
      "{.arg user_agent} must be a single non-empty character string"
    )
  }

  clean_text(user_agent)
}

#' Return the configured user agent
#' @noRd
request_user_agent <- function() {
  option_user_agent <- getOption("tezr.user_agent")
  env_user_agent <- Sys.getenv("TEZR_USER_AGENT", unset = NA_character_)

  if (!is.null(option_user_agent)) {
    return(validate_user_agent(option_user_agent))
  }
  if (!is.na(env_user_agent) && nchar(env_user_agent) > 0L) {
    return(validate_user_agent(env_user_agent))
  }

  package_user_agent()
}

#' Parse common logical values from environment variables
#' @noRd
parse_env_logical <- function(value, default) {
  if (is.na(value) || nchar(value) == 0L) {
    return(default)
  }

  normalized <- stringr::str_to_lower(clean_text(value))
  if (normalized %in% c("true", "1", "yes", "y", "on")) {
    return(TRUE)
  }
  if (normalized %in% c("false", "0", "no", "n", "off")) {
    return(FALSE)
  }

  default
}

#' Return whether informational package messages should be shown
#' @noRd
tezr_verbose <- function() {
  option_verbose <- getOption("tezr.verbose")
  if (!is.null(option_verbose)) {
    if (
      !is.logical(option_verbose) ||
        length(option_verbose) != 1L ||
        is.na(option_verbose)
    ) {
      cli::cli_abort("{.option tezr.verbose} must be TRUE or FALSE")
    }
    return(option_verbose)
  }

  parse_env_logical(Sys.getenv("TEZR_VERBOSE", unset = NA_character_), TRUE)
}

#' Emit an informational alert when verbosity is enabled
#' @noRd
tezr_inform <- function(..., .envir = parent.frame()) {
  if (tezr_verbose()) {
    cli::cli_alert_info(..., .envir = .envir)
  }
  invisible(NULL)
}

#' Emit a success alert when verbosity is enabled
#' @noRd
tezr_success <- function(..., .envir = parent.frame()) {
  if (tezr_verbose()) {
    cli::cli_alert_success(..., .envir = .envir)
  }
  invisible(NULL)
}

#' Return current request configuration
#' @noRd
current_request_config <- function() {
  list(
    user_agent = request_user_agent(),
    verbose = tezr_verbose(),
    rate_limit_seconds = 2,
    retry_max_tries = 3,
    session_refresh_requests = 50L,
    session_refresh_minutes = 20L
  )
}

#' Validate a request configuration logical argument
#' @noRd
validate_config_logical <- function(value, arg) {
  if (
    !is.logical(value) ||
      length(value) != 1L ||
      is.na(value)
  ) {
    cli::cli_abort("{.arg {arg}} must be TRUE or FALSE")
  }

  value
}

#' Reset request configuration options
#' @noRd
reset_request_config <- function() {
  options(tezr.user_agent = NULL)
  options(tezr.verbose = NULL)
  invisible(NULL)
}

#' Apply a request user agent option
#' @noRd
set_request_user_agent <- function(user_agent) {
  if (is.null(user_agent)) {
    return(invisible(NULL))
  }

  options(tezr.user_agent = validate_user_agent(user_agent))
  invisible(NULL)
}

#' Apply a request verbosity option
#' @noRd
set_request_verbose <- function(verbose) {
  if (is.null(verbose)) {
    return(invisible(NULL))
  }

  options(tezr.verbose = validate_config_logical(verbose, "verbose"))
  invisible(NULL)
}

#' Configure request behavior
#'
#' Sets package-level request options. `tezr` identifies itself with a
#' package-specific user agent by default. You can override that header when
#' required by a network policy or by the National Thesis Center portal.
#'
#' Informational messages are shown by default. Set `verbose = FALSE` to
#' silence progress and success messages. Warnings and errors are still shown.
#'
#' You can also set `TEZR_USER_AGENT` and `TEZR_VERBOSE` before starting R.
#' Explicit R options take precedence over environment variables.
#'
#' @param user_agent Character. Optional user agent to send with HTTP requests.
#' @param verbose Logical. Show informational request messages?
#' @param reset Logical. Reset request options to package defaults before
#'   applying other arguments.
#'
#' @return Invisible list with the active request configuration.
#' @family request configuration
#' @export
#'
#' @examples
#' # Silence progress messages for the current R session
#' request_config(verbose = FALSE)
#'
#' # Set a custom user agent for an institutional network policy
#' request_config(user_agent = "my-lab-contact@example.edu")
#'
#' # Return to package defaults
#' request_config(reset = TRUE)
request_config <- function(
  user_agent = NULL,
  verbose = NULL,
  reset = FALSE
) {
  reset <- validate_config_logical(reset, "reset")
  if (reset) {
    reset_request_config()
  }
  set_request_user_agent(user_agent)
  set_request_verbose(verbose)

  invisible(current_request_config())
}

#' Return HTTP headers used for all requests
#' @noRd
default_request_headers <- function() {
  c(
    "User-Agent" = request_user_agent(),
    "Accept" = paste0(
      "text/html,application/xhtml+xml,application/xml;q=0.9,",
      "*/*;q=0.8"
    ),
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

    tezr_inform(paste0("Refreshing session (", reason, ")..."))
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
