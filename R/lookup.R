lookup_cache <- new.env(parent = emptyenv())

#' Normalize lookup labels for robust Turkish name matching
#' @noRd
normalize_lookup_label <- function(text, compact = FALSE) {
  normalized <- clean_text(text)
  normalized <- stringi::stri_trans_tolower(normalized, locale = "tr")
  normalized <- stringi::stri_trans_general(normalized, "Latin-ASCII")
  normalized <- gsub("[^A-Za-z0-9]+", " ", normalized, perl = TRUE)
  normalized <- clean_text(normalized)

  if (isTRUE(compact)) {
    normalized <- gsub("\\s+", "", normalized, perl = TRUE)
  }

  normalized
}

#' Return source labels for lookup matching
#' @noRd
lookup_source_labels <- function(items) {
  labels <- if ("name" %in% names(items)) {
    items$name
  } else {
    items$clean_name
  }

  if ("clean_name" %in% names(items)) {
    missing_labels <- is.na(labels) | !nzchar(labels)
    labels[missing_labels] <- items$clean_name[missing_labels]
  }

  labels
}

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
        clean_name = clean_text(.data$name) |> stringr::str_to_lower(),
        normalized_name = normalize_lookup_label(.data$name),
        compact_name = normalize_lookup_label(.data$name, compact = TRUE)
      )

    lookup_cache[[cache_key]] <- filter_options
    return(filter_options)
  }

  return(tibble::tibble(
    name = character(),
    id = character(),
    clean_name = character(),
    normalized_name = character(),
    compact_name = character()
  ))
}

#' Resolve a human-readable name to its API ID via exact or partial match
#' @noRd
generic_lookup_item <- function(name, fetch_fn) {
  if (is.null(name) || length(name) == 0L || !nzchar(name[[1L]])) {
    return(NULL)
  }

  items <- fetch_fn()
  name_lower <- stringr::str_to_lower(clean_text(name))
  normalized_name <- normalize_lookup_label(name)
  compact_name <- normalize_lookup_label(name, compact = TRUE)

  matched_items <- items |> dplyr::filter(.data$clean_name == name_lower)

  if (nrow(matched_items) == 0) {
    lookup_names <- if ("normalized_name" %in% names(items)) {
      items$normalized_name
    } else {
      normalize_lookup_label(lookup_source_labels(items))
    }

    match_mask <- !is.na(lookup_names) & lookup_names == normalized_name
    matched_items <- items[match_mask, , drop = FALSE]
  }

  if (nrow(matched_items) == 0) {
    matched_items <- items |>
      dplyr::filter(stringr::str_detect(
        .data$clean_name,
        stringr::fixed(name_lower)
      ))
  }

  if (nrow(matched_items) == 0) {
    lookup_names <- if ("normalized_name" %in% names(items)) {
      items$normalized_name
    } else {
      normalize_lookup_label(lookup_source_labels(items))
    }

    match_mask <- !is.na(lookup_names) &
      stringr::str_detect(lookup_names, stringr::fixed(normalized_name))
    matched_items <- items[match_mask, , drop = FALSE]
  }

  if (nrow(matched_items) == 0) {
    lookup_names <- if ("compact_name" %in% names(items)) {
      items$compact_name
    } else {
      normalize_lookup_label(lookup_source_labels(items), compact = TRUE)
    }

    match_mask <- !is.na(lookup_names) & lookup_names == compact_name
    matched_items <- items[match_mask, , drop = FALSE]
  }

  if (nrow(matched_items) == 0) {
    lookup_names <- if ("compact_name" %in% names(items)) {
      items$compact_name
    } else {
      normalize_lookup_label(lookup_source_labels(items), compact = TRUE)
    }

    match_mask <- !is.na(lookup_names) &
      stringr::str_detect(lookup_names, stringr::fixed(compact_name))
    matched_items <- items[match_mask, , drop = FALSE]
  }

  if (nrow(matched_items) > 0) {
    return(matched_items[1, , drop = FALSE])
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

#' Resolve a university name to its API item
#' @noRd
lookup_university_item <- function(name) {
  generic_lookup_item(name, fetch_university_list)
}

#' Resolve an institute name to its API item
#' @noRd
lookup_institute_item <- function(name) {
  generic_lookup_item(name, fetch_institute_list)
}

#' Resolve a division name to its API item
#' @noRd
lookup_division_item <- function(name) {
  generic_lookup_item(name, fetch_division_list)
}

#' Resolve a discipline name to its API item
#' @noRd
lookup_discipline_item <- function(name) {
  generic_lookup_item(name, fetch_discipline_list)
}

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
