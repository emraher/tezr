#' Trim whitespace and collapse internal runs of whitespace
#' @noRd
clean_text <- function(text) {
  if (length(text) == 0) {
    return(text)
  }

  trimmed_text <- trimws(text)
  squished_text <- gsub("[[:space:]]+", " ", trimmed_text, perl = TRUE)

  return(squished_text)
}

#' Parse semicolon-separated bilingual entries
#' @noRd
parse_bilingual_entries <- function(text) {
  if (is.na(text) || nchar(text) == 0) {
    return(list(tr = character(), en = character()))
  }

  items <- strsplit(text, ";", fixed = TRUE)[[1]]
  items <- clean_text(items)
  items <- items[nchar(items) > 0]

  if (length(items) == 0) {
    return(list(tr = character(), en = character()))
  }

  equals_positions <- regexpr("=", items, fixed = TRUE)
  has_equals <- equals_positions > 0
  item_lengths <- nchar(items)

  tr_values <- items
  en_values <- rep(NA_character_, length(items))

  if (any(has_equals)) {
    tr_values[has_equals] <- substr(
      items[has_equals],
      1L,
      equals_positions[has_equals] - 1L
    )
    en_values[has_equals] <- substr(
      items[has_equals],
      equals_positions[has_equals] + 1L,
      item_lengths[has_equals]
    )

    tr_values <- clean_text(tr_values)
    en_values[has_equals] <- clean_text(en_values[has_equals])
  }

  list(tr = tr_values, en = en_values)
}

#' Collapse parsed bilingual entries into subject fields
#' @noRd
split_bilingual_subjects <- function(text) {
  parsed <- parse_bilingual_entries(text)

  collapse_subjects <- function(values) {
    values <- values[!is.na(values)]
    if (length(values) == 0) {
      return(NA_character_)
    }
    paste(values, collapse = "; ")
  }

  list(
    subject_tr = collapse_subjects(parsed$tr),
    subject_en = collapse_subjects(parsed$en)
  )
}

#' Strip academic titles (Prof. Dr., Doc. Dr., etc.) and normalize case
#' @noRd
clean_advisor_name <- function(name) {
  if (is.null(name) || length(name) == 0) {
    return("")
  }

  titles <- c(
    "Prof\\. Dr\\.",
    "Do\\u00E7\\. Dr\\.",
    "Dr\\. \\u00D6\\u011Fr\\. \\u00DCyesi",
    "Dr\\.",
    "\\u00D6\\u011Fr\\. G\\u00F6r\\.",
    "Ar\\u015F\\. G\\u00F6r\\.",
    "Prof\\.",
    "Do\\u00E7\\."
  )

  pattern <- paste0("^((", paste(titles, collapse = "|"), "))\\s*")

  name |>
    stringr::str_remove_all(stringr::regex(pattern, ignore_case = TRUE)) |>
    clean_text() |>
    stringr::str_to_upper(locale = "tr")
}

#' Lowercase and strip Turkish diacritics for case-insensitive matching
#' @noRd
normalize_language_label <- function(text) {
  if (is.null(text)) {
    return(NA_character_)
  }

  cleaned <- clean_text(text)
  cleaned[nchar(cleaned) == 0] <- NA_character_

  lowered <- stringr::str_to_lower(cleaned, locale = "tr")
  stringr::str_replace_all(
    lowered,
    c(
      "\u0131" = "i",
      "\u015f" = "s",
      "\u011f" = "g",
      "\u00fc" = "u",
      "\u00f6" = "o",
      "\u00e7" = "c"
    )
  )
}

#' Validate optional language IDs or labels
#' @noRd
validate_language_values <- function(language, allow_multiple = FALSE) {
  if (is.null(language)) {
    return(NULL)
  }

  if (
    length(language) == 0L ||
      any(is.na(language)) ||
      (!isTRUE(allow_multiple) && length(language) != 1L)
  ) {
    cli::cli_abort("{.arg language} must be a valid language id or label")
  }

  if (is.numeric(language)) {
    is_whole <- vapply(
      language,
      is_whole_number_value,
      logical(1)
    )
    if (!all(is_whole)) {
      cli::cli_abort("{.arg language} must be a valid language id or label")
    }
    return(language)
  }

  if (!is.character(language)) {
    cli::cli_abort("{.arg language} must be a numeric id or character label")
  }

  cleaned <- clean_text(language)
  if (any(!nzchar(cleaned))) {
    cli::cli_abort("{.arg language} must be a valid language id or label")
  }

  cleaned
}

#' Test whether a scalar value is written as a whole positive integer
#' @noRd
is_whole_number_value <- function(value) {
  if (length(value) != 1L || is.na(value)) {
    return(FALSE)
  }

  value_text <- clean_text(as.character(value))
  !is.na(value_text) && grepl("^[0-9]+$", value_text)
}

#' Convert a language name, abbreviation, or numeric ID to its API integer ID
#' @noRd
resolve_language_id <- function(language) {
  language <- validate_language_values(language, allow_multiple = FALSE)

  if (is.null(language)) {
    return(NULL)
  }

  if (is.numeric(language)) {
    language_id <- suppressWarnings(as.integer(clean_text(as.character(language))))
    if (is.na(language_id)) {
      cli::cli_abort("{.arg language} must be a valid language id")
    }

    valid_ids <- suppressWarnings(as.integer(languages$value))
    if (!language_id %in% valid_ids) {
      cli::cli_abort("{.arg language} must be a valid language id")
    }

    return(language_id)
  }

  lang <- clean_text(language)
  target <- normalize_language_label(lang)

  if (stringr::str_detect(target, "^\\d+$")) {
    return(resolve_language_id(as.integer(target)))
  }

  # ISO 639-1 two-letter codes for all languages in the YOK database
  iso_codes <- c(
    tr = "turkish",
    en = "english",
    ar = "arabic",
    de = "german",
    fr = "french",
    es = "spanish",
    it = "italian",
    ru = "russian",
    pl = "polish",
    zh = "chinese",
    ku = "kurdish",
    az = "azerbaijanese",
    bg = "bulgarian",
    cs = "czech",
    ro = "romanian",
    nl = "dutch",
    ja = "japanese",
    fa = "persian",
    el = "greek",
    sl = "slovenian",
    mk = "macedonian",
    ky = "kirghiz",
    bs = "bosnian",
    ka = "georgian",
    ko = "korean",
    hy = "armenian",
    ms = "malay",
    kk = "kazakh",
    uk = "ukrainian",
    mn = "mongolian",
    id = "indonesian",
    uz = "uzbek",
    hu = "hungarian",
    sr = "serbian",
    pt = "portuguese",
    sq = "albanian",
    lv = "latvian",
    ady = "adyghe",
    zza = "zaza"
  )

  if (target %in% names(iso_codes)) {
    target <- iso_codes[[target]]
  }

  labels_tr <- normalize_language_label(languages$label_tr)
  labels_en <- normalize_language_label(languages$label_en)

  match_idx <- which(labels_tr == target | labels_en == target)
  if (length(match_idx) == 0) {
    cli::cli_abort("{.arg language} must be a valid language id or label")
  }

  language_id <- suppressWarnings(as.integer(languages$value[match_idx[1]]))
  if (is.na(language_id)) {
    cli::cli_abort("{.arg language} must be a valid language id")
  }

  return(language_id)
}

#' Validate and default year_start, year_end, and language for search queries
#' @noRd
coerce_search_fields <- function(
  year_start = NULL,
  year_end = NULL,
  language = NULL
) {
  year_start <- validate_year(year_start, "year_start")
  year_end <- validate_year(year_end, "year_end")

  # If only one bound is provided, default the other to a sensible range.
  # The server treats a missing bound as 0, which can yield empty results.
  if (is.null(year_start) && !is.null(year_end)) {
    year_start <- 1959L
  }
  if (!is.null(year_start) && is.null(year_end)) {
    year_end <- as.integer(format(Sys.Date(), "%Y"))
  }

  list(
    year_start = year_start,
    year_end = year_end,
    language_id = resolve_language_id(language)
  )
}

#' Validate year parameter
#'
#' @param year Numeric or integer value to validate
#' @param param_name Name of the parameter for error messages
#' @return The validated year as integer
#' @noRd
validate_year <- function(year, param_name = "year") {
  if (is.null(year)) {
    return(NULL)
  }

  if (length(year) != 1) {
    cli::cli_abort("{.arg {param_name}} must be a single year")
  }

  if (!is_whole_number_value(year)) {
    cli::cli_abort("{.arg {param_name}} must be a valid year")
  }

  year_int <- suppressWarnings(as.integer(clean_text(as.character(year))))

  if (is.na(year_int)) {
    cli::cli_abort("{.arg {param_name}} must be a valid year")
  }

  current_year <- as.integer(format(Sys.Date(), "%Y"))

  if (year_int < 1959 || year_int > current_year) {
    cli::cli_abort(
      "{.arg {param_name}} must be between 1959 and {current_year}"
    )
  }

  return(year_int)
}

#' Validate an optional scalar label argument
#' @noRd
validate_optional_label <- function(value, arg_name) {
  validate_text_values(value, arg_name, allow_multiple = FALSE)
}

#' Validate optional non-empty character values
#' @noRd
validate_text_values <- function(
  value,
  arg_name,
  allow_multiple = TRUE,
  coerce = FALSE
) {
  if (is.null(value)) {
    return(NULL)
  }

  if (isTRUE(coerce)) {
    value <- as.character(value)
  }

  invalid_shape <- !is.character(value) ||
    length(value) == 0L ||
    any(is.na(value)) ||
    (!isTRUE(allow_multiple) && length(value) != 1L)

  if (invalid_shape) {
    if (isTRUE(allow_multiple)) {
      cli::cli_abort("{.arg {arg_name}} must contain non-empty character values")
    }
    cli::cli_abort(
      "{.arg {arg_name}} must be a single non-empty character string"
    )
  }

  cleaned <- clean_text(value)
  if (any(!nzchar(cleaned))) {
    if (isTRUE(allow_multiple)) {
      cli::cli_abort("{.arg {arg_name}} must contain non-empty character values")
    }
    cli::cli_abort(
      "{.arg {arg_name}} must be a single non-empty character string"
    )
  }

  cleaned
}

#' Validate an optional scalar positive integer ID
#' @noRd
validate_optional_id <- function(value, arg_name) {
  if (is.null(value)) {
    return(NULL)
  }
  if (length(value) != 1) {
    cli::cli_abort("{.arg {arg_name}} must be a single positive integer")
  }
  if (!is_whole_number_value(value)) {
    cli::cli_abort("{.arg {arg_name}} must be a single positive integer")
  }

  id <- suppressWarnings(as.integer(clean_text(as.character(value))))
  if (is.na(id) || id <= 0L) {
    cli::cli_abort("{.arg {arg_name}} must be a single positive integer")
  }
  return(id)
}

#' Validate a scalar logical cache flag
#' @noRd
validate_ignore_cache <- function(ignore_cache) {
  if (
    !is.logical(ignore_cache) ||
      length(ignore_cache) != 1 ||
      is.na(ignore_cache)
  ) {
    cli::cli_abort("{.arg ignore_cache} must be TRUE or FALSE")
  }
  invisible(ignore_cache)
}

#' Validate a scalar positive result limit or Inf
#' @noRd
validate_max_search_results <- function(max_search_results) {
  if (
    !is.numeric(max_search_results) ||
      length(max_search_results) != 1 ||
      is.na(max_search_results) ||
      max_search_results <= 0
  ) {
    cli::cli_abort(
      "{.arg max_search_results} must be a single positive integer or {.val Inf}"
    )
  }

  if (is.infinite(max_search_results)) {
    return(max_search_results)
  }

  if (
    max_search_results > .Machine$integer.max ||
      max_search_results != as.integer(max_search_results)
  ) {
    cli::cli_abort(
      "{.arg max_search_results} must be a single positive integer or {.val Inf}"
    )
  }

  as.integer(max_search_results)
}

#' Coalesce NULL, NA, and empty string values
#'
#' Stricter than rlang::`%||%` — also replaces NA and empty strings.
#' Named differently to avoid shadowing rlang::`%||%`.
#' @noRd
`%|na|%` <- function(x, y) {
  if (is.null(x) || is.na(x) || (is.character(x) && nchar(x) == 0)) {
    return(y)
  }
  return(x)
}
