#' Parse search results from HTML
#'
#' Parses the watable-based results from tezSorguSonucYeni.jsp.
#' The data is embedded in JavaScript objects within the page, not in HTML tables.
#'
#' @param html An xml_document or xml_node object containing search results
#' @return A tibble with parsed thesis records
#' @noRd
parse_results_table <- function(html) {
  # Get the raw HTML text - data is in JavaScript, not HTML tables
  html_text <- as.character(html)

  doc_matches <- extract_js_doc_blocks(html_text)

  if (length(doc_matches) == 0) {
    return(empty_results_tibble())
  }

  parsed_records <- vector("list", length(doc_matches))
  parsed_count <- 0L

  for (doc_index in seq_along(doc_matches)) {
    parsed_record <- parse_js_doc(doc_matches[[doc_index]])
    if (!is.null(parsed_record)) {
      parsed_count <- parsed_count + 1L
      parsed_records[[parsed_count]] <- parsed_record
    }
  }

  if (parsed_count == 0) {
    return(empty_results_tibble())
  }

  parsed_records <- parsed_records[seq_len(parsed_count)]

  return(build_results_tibble(parsed_records))
}

#' Extract JavaScript doc object blocks from search-results HTML
#' @noRd
extract_js_doc_blocks <- function(html_text) {
  # Prefer blocks that explicitly terminate with rows.push(doc); this avoids
  # truncation when field text contains "}" and is much faster on large pages.
  block_pattern <- stringr::regex(
    "var doc = \\{[\\s\\S]*?\\};\\s*rows\\.push\\(doc\\);",
    dotall = TRUE
  )
  doc_blocks <- stringr::str_extract_all(html_text, block_pattern)[[1]]

  if (length(doc_blocks) == 0) {
    fallback_pattern <- stringr::regex(
      "var doc = \\{[\\s\\S]*?\\};",
      dotall = TRUE
    )
    doc_blocks <- stringr::str_extract_all(html_text, fallback_pattern)[[1]]
    return(doc_blocks)
  }

  doc_blocks <- stringr::str_replace(
    doc_blocks,
    stringr::regex("\\s*rows\\.push\\(doc\\);\\s*$"),
    ""
  )

  return(doc_blocks)
}

#' Return a zero-row tibble with the standard results column schema
#' @noRd
empty_results_tibble <- function() {
  return(tibble::tibble(
    thesis_no = character(),
    title_original = character(),
    title_translation = character(),
    author = character(),
    university = character(),
    year = integer(),
    thesis_type_tr = character(),
    thesis_type_en = character(),
    language_tr = character(),
    language_en = character(),
    subject_tr = character(),
    subject_en = character(),
    detail_id = character()
  ))
}

#' Build a results tibble from parsed JavaScript record lists
#' @noRd
build_results_tibble <- function(parsed_records) {
  record_count <- length(parsed_records)

  thesis_no <- character(record_count)
  title_original <- character(record_count)
  title_translation <- character(record_count)
  author <- character(record_count)
  university <- character(record_count)
  year <- integer(record_count)
  thesis_type_tr <- character(record_count)
  thesis_type_en <- character(record_count)
  language_tr <- character(record_count)
  language_en <- character(record_count)
  subject_tr <- character(record_count)
  subject_en <- character(record_count)
  detail_id <- character(record_count)

  for (record_index in seq_len(record_count)) {
    parsed_record <- parsed_records[[record_index]]

    thesis_no[[record_index]] <- parsed_record$thesis_no %|na|% NA_character_
    title_original[[record_index]] <- parsed_record$title_original %|na|%
      NA_character_
    title_translation[[record_index]] <- parsed_record$title_translation %|na|%
      NA_character_
    author[[record_index]] <- parsed_record$author %|na|% NA_character_
    university[[record_index]] <- parsed_record$university %|na|% NA_character_
    year[[record_index]] <- parsed_record$year %|na|% NA_integer_
    thesis_type_tr[[record_index]] <- parsed_record$thesis_type_tr %|na|%
      NA_character_
    thesis_type_en[[record_index]] <- parsed_record$thesis_type_en %|na|%
      NA_character_
    language_tr[[record_index]] <- parsed_record$language_tr %|na|%
      NA_character_
    language_en[[record_index]] <- parsed_record$language_en %|na|%
      NA_character_
    subject_tr[[record_index]] <- parsed_record$subject_tr %|na|% NA_character_
    subject_en[[record_index]] <- parsed_record$subject_en %|na|% NA_character_
    detail_id[[record_index]] <- parsed_record$detail_id %|na|% NA_character_
  }

  return(tibble::tibble(
    thesis_no = thesis_no,
    title_original = title_original,
    title_translation = title_translation,
    author = author,
    university = university,
    year = year,
    thesis_type_tr = thesis_type_tr,
    thesis_type_en = thesis_type_en,
    language_tr = language_tr,
    language_en = language_en,
    subject_tr = subject_tr,
    subject_en = subject_en,
    detail_id = detail_id
  ))
}

#' Parse JavaScript object fields into a named character vector
#' @noRd
parse_js_doc_fields <- function(doc_str) {
  field_matches <- stringr::str_match_all(
    doc_str,
    "([A-Za-z][A-Za-z0-9_]*)\\s*:\\s*\"((?:[^\"\\\\]|\\\\.)*)\""
  )[[1]]

  if (nrow(field_matches) == 0) {
    return(stats::setNames(character(), character()))
  }

  field_names <- field_matches[, 2]
  field_values <- normalize_js_field_values(field_matches[, 3])

  return(stats::setNames(field_values, field_names))
}

#' Decode escaped JavaScript field values and normalize whitespace
#' @noRd
normalize_js_field_values <- function(field_values) {
  if (length(field_values) == 0) {
    return(field_values)
  }

  has_backslash <- !is.na(field_values) &
    grepl("\\", field_values, fixed = TRUE)

  if (any(has_backslash)) {
    escaped_values <- field_values[has_backslash]
    escaped_values <- gsub("\\\\n", " ", escaped_values, fixed = TRUE)
    escaped_values <- gsub("\\n", " ", escaped_values, fixed = TRUE)
    escaped_values <- gsub("\\\\'", "'", escaped_values, fixed = TRUE)
    escaped_values <- gsub("\\'", "'", escaped_values, fixed = TRUE)
    escaped_values <- gsub("\\\\\"", "\"", escaped_values, fixed = TRUE)
    escaped_values <- gsub("\\\"", "\"", escaped_values, fixed = TRUE)
    field_values[has_backslash] <- escaped_values
  }

  needs_whitespace_clean <- !is.na(field_values) &
    grepl(
      "^\\s|\\s$|\\s{2,}",
      field_values,
      perl = TRUE
    )

  if (any(needs_whitespace_clean)) {
    field_values[needs_whitespace_clean] <- clean_text(field_values[
      needs_whitespace_clean
    ])
  }

  return(field_values)
}

#' Strip HTML tags only for values that contain tags
#' @noRd
strip_html_tags_if_present <- function(values) {
  if (length(values) == 0) {
    return(values)
  }

  has_tags <- !is.na(values) & grepl("<", values, fixed = TRUE)

  if (any(has_tags)) {
    values[has_tags] <- gsub("<[^>]+>", "", values[has_tags], perl = TRUE)
  }

  return(values)
}

#' Return one cleaned field value from parsed JavaScript fields
#' @noRd
get_doc_field <- function(parsed_fields, field_name) {
  field_value <- parsed_fields[field_name]

  if (length(field_value) == 0 || is.na(field_value[[1]])) {
    return(NA_character_)
  }

  return(field_value[[1]])
}

#' Parse title field into primary and secondary strings
#' @noRd
parse_title_fields_fast <- function(title_raw) {
  if (is.na(title_raw) || nchar(title_raw) == 0) {
    return(list(primary = NA_character_, secondary = NA_character_))
  }

  br_match <- regexpr("<br\\s*/?>", title_raw, perl = TRUE)
  br_start <- br_match[1]
  br_length <- attr(br_match, "match.length")[1]

  if (br_start > 0) {
    title_primary_raw <- substr(title_raw, 1, br_start - 1)
    title_secondary_raw <- substr(
      title_raw,
      br_start + br_length,
      nchar(title_raw)
    )
  } else {
    title_primary_raw <- title_raw
    title_secondary_raw <- NA_character_
  }

  title_values <- c(title_primary_raw, title_secondary_raw)
  title_values <- strip_html_tags_if_present(title_values)
  title_primary <- clean_text(title_values[[1]])
  title_secondary <- if (!is.na(title_secondary_raw)) {
    clean_text(title_values[[2]])
  } else {
    NA_character_
  }

  return(list(
    primary = title_primary %|na|% NA_character_,
    secondary = title_secondary %|na|% NA_character_
  ))
}

#' Parse a single JavaScript document object into a named record list
#' @noRd
parse_js_doc <- function(doc_str) {
  parsed_fields <- parse_js_doc_fields(doc_str)
  userid_html <- get_doc_field(parsed_fields, "userId")

  if (is.na(userid_html)) {
    return(NULL)
  }

  thesis_no <- stringr::str_match(
    userid_html,
    ">([0-9]+)(?:</span>)?$"
  )[2]

  detail_match <- stringr::str_match(
    userid_html,
    "tezDetay\\('([^']+)'\\s*,\\s*'([^']+)'\\)"
  )
  detail_id <- if (!is.na(detail_match[1])) detail_match[2] else NA_character_

  author <- get_doc_field(parsed_fields, "name")
  year <- suppressWarnings(as.integer(get_doc_field(parsed_fields, "age")))

  title_raw <- get_doc_field(parsed_fields, "weight")
  parsed_title <- parse_title_fields_fast(title_raw %|na|% "")

  # Extract and split subject into Turkish and English
  subject_raw <- get_doc_field(parsed_fields, "someDate") %|na|% NA_character_
  subjects <- split_bilingual_subjects(subject_raw)

  # University and language fields - YÖK may not return these in basic search
  # but we include them for consistency with advanced search
  university <- get_doc_field(parsed_fields, "uni") %|na|% NA_character_
  language_raw <- get_doc_field(parsed_fields, "height") %|na|% ""
  language <- extract_stat_language(language_raw)

  thesis_type <- extract_stat_thesis_type(get_doc_field(
    parsed_fields,
    "important"
  ))

  return(list(
    thesis_no = thesis_no %|na|% NA_character_,
    title_original = parsed_title$primary %|na|% NA_character_,
    title_translation = parsed_title$secondary %|na|% NA_character_,
    author = author %|na|% NA_character_,
    university = university,
    year = year %|na|% NA_integer_,
    thesis_type_tr = thesis_type$tr %|na|% NA_character_,
    thesis_type_en = thesis_type$en %|na|% NA_character_,
    language_tr = language$tr,
    language_en = language$en,
    subject_tr = subjects$subject_tr,
    subject_en = subjects$subject_en,
    detail_id = detail_id
  ))
}
