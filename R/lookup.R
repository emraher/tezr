lookup_cache <- new.env(parent = emptyenv())

#' Fetch an entity list from the API and cache it by type
#' @noRd
generic_fetch_list <- function(cache_key, endpoint) {
  if (!is.null(lookup_cache[[cache_key]])) {
    return(lookup_cache[[cache_key]])
  }

  resp <- create_session() |>
    httr2::req_url(paste0(base_url, endpoint)) |>
    httr2::req_perform()

  html <- httr2::resp_body_html(resp)
  links <- html |> rvest::html_elements("a[href^='javascript:eklecikar']")
  hrefs <- rvest::html_attr(links, "href")
  matches <- stringr::str_match(hrefs, "eklecikar\\('([^']+)',\\s*'([^']+)'")

  if (nrow(matches) > 0) {
    filter_options <- tibble::tibble(
      name = matches[, 2],
      id = matches[, 3]
    ) |>
      dplyr::mutate(
        clean_name = clean_text(.data$name) |> stringr::str_to_lower()
      )

    lookup_cache[[cache_key]] <- filter_options
    return(filter_options)
  }

  return(tibble::tibble(
    name = character(),
    id = character(),
    clean_name = character()
  ))
}

#' Resolve a human-readable name to its API ID via exact or partial match
#' @noRd
generic_lookup_id <- function(name, fetch_fn) {
  if (is.null(name) || length(name) == 0L || !nzchar(name[[1L]])) {
    return(NULL)
  }

  items <- fetch_fn()
  name_lower <- stringr::str_to_lower(clean_text(name))

  matched_items <- items |> dplyr::filter(.data$clean_name == name_lower)

  if (nrow(matched_items) == 0) {
    matched_items <- items |>
      dplyr::filter(stringr::str_detect(
        .data$clean_name,
        stringr::fixed(name_lower)
      ))
  }

  if (nrow(matched_items) > 0) {
    return(matched_items$id[1])
  }

  return(NULL)
}

# Lookup configuration: maps entity type to endpoint
lookup_config <- list(
  university = "uniEkle.jsp",
  institute = "ensEkle.jsp",
  division = "abdEkle.jsp",
  discipline = "bilimDaliEkle.jsp",
  subject = "konEkle.jsp"
)

#' Fetch all universities from the API
#' @noRd
fetch_university_list <- function() {
  generic_fetch_list("university", lookup_config$university)
}

#' Fetch all institutes from the API
#' @noRd
fetch_institute_list <- function() {
  generic_fetch_list("institute", lookup_config$institute)
}

#' Fetch all divisions from the API
#' @noRd
fetch_division_list <- function() {
  generic_fetch_list("division", lookup_config$division)
}

#' Fetch all disciplines from the API
#' @noRd
fetch_discipline_list <- function() {
  generic_fetch_list("discipline", lookup_config$discipline)
}

#' Fetch all subjects from the API
#' @noRd
fetch_subject_list <- function() {
  generic_fetch_list("subject", lookup_config$subject)
}

#' Resolve a university name to its API ID
#' @noRd
lookup_university_id <- function(name) {
  generic_lookup_id(name, fetch_university_list)
}

#' Resolve an institute name to its API ID
#' @noRd
lookup_institute_id <- function(name) {
  generic_lookup_id(name, fetch_institute_list)
}

#' Resolve a division name to its API ID
#' @noRd
lookup_division_id <- function(name) {
  generic_lookup_id(name, fetch_division_list)
}

#' Resolve a discipline name to its API ID
#' @noRd
lookup_discipline_id <- function(name) {
  generic_lookup_id(name, fetch_discipline_list)
}
#' Resolve a subject name to its API ID
#' @noRd
lookup_subject_id <- function(name) generic_lookup_id(name, fetch_subject_list)

#' Return a cleaned two-column lookup table
#' @noRd
list_lookup_values <- function(fetch_fn) {
  fetch_fn() |>
    dplyr::select("name", "id") |>
    dplyr::mutate(name = clean_text(.data$name)) |>
    tidyr::drop_na()
}

#' List all available universities
#'
#' Returns all universities in the National Thesis Center database.
#' Turkish: Üniversite
#'
#' @return A tibble with two columns:
#'   \itemize{
#'     \item name - Character. University name
#'     \item id - Character. Internal API identifier
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' unis <- list_universities()
#' }
list_universities <- function() {
  list_lookup_values(fetch_university_list)
}

#' List all available institutes
#'
#' Returns all institutes in the National Thesis Center database.
#' Turkish: Enstitü
#'
#' @return A tibble with two columns:
#'   \itemize{
#'     \item name - Character. Institute name
#'     \item id - Character. Internal API identifier
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' insts <- list_institutes()
#' }
list_institutes <- function() {
  list_lookup_values(fetch_institute_list)
}

#' List all available divisions
#'
#' Returns all divisions (Anabilim Dalı values) in the National Thesis Center database.
#' Turkish: Anabilim Dalı (ABD)
#'
#' @return A tibble with two columns:
#'   \itemize{
#'     \item name - Character. Division name
#'     \item id - Character. Internal API identifier
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' divisions <- list_divisions()
#' }
list_divisions <- function() {
  list_lookup_values(fetch_division_list)
}

#' List all available disciplines
#'
#' Returns all disciplines (Bilim Dalı values) in the National Thesis Center database.
#' Turkish: Bilim Dalı
#'
#' @return A tibble with two columns:
#'   \itemize{
#'     \item name - Character. Discipline name
#'     \item id - Character. Internal API identifier
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' disciplines <- list_disciplines()
#' }
list_disciplines <- function() {
  list_lookup_values(fetch_discipline_list)
}

#' List all available subjects
#'
#' Returns all subjects in the National Thesis Center database.
#' Turkish: Konu
#'
#' @return A tibble with three columns:
#'   \itemize{
#'     \item name_tr - Character. Turkish subject name
#'     \item name_en - Character. English subject name
#'     \item id - Character. Internal API identifier
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' subjects <- list_subjects()
#' }
list_subjects <- function() {
  return(
    fetch_subject_list() |>
      tidyr::separate_wider_delim(
        "name",
        " = ",
        names = c("name_tr", "name_en")
      ) |>
      dplyr::mutate(
        name_tr = clean_text(.data$name_tr),
        name_en = clean_text(.data$name_en)
      ) |>
      dplyr::select("name_tr", "name_en", "id") |>
      tidyr::drop_na()
  )
}
