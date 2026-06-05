#' Parse search results from HTML
#'
#' Parses the watable-based results from tezSorguSonucYeni.jsp.
#' The data is embedded in JavaScript objects within the page, not in HTML
#' tables.
#'
#' @param html An xml_document or xml_node object containing search results
#' @return A tibble with parsed thesis records
#' @noRd
parse_results_table <- function(html) {
  modern_results <- parse_modern_result_cards(html)
  if (nrow(modern_results) > 0) {
    return(modern_results)
  }

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

#' Compose an opaque detail ID from one or two portal identifiers
#' @noRd
compose_detail_id <- function(id, no = NA_character_) {
  id <- coalesce_missing(id, NA_character_)
  no <- coalesce_missing(no, NA_character_)

  if (is.na(id)) {
    return(NA_character_)
  }
  if (is.na(no)) {
    return(id)
  }

  paste(id, no, sep = "|")
}

#' Split an opaque detail ID into portal request identifiers
#' @noRd
split_detail_id <- function(detail_id) {
  detail_id <- coalesce_missing(detail_id, NA_character_)
  if (is.na(detail_id)) {
    return(list(id = NA_character_, no = NA_character_))
  }

  parts <- strsplit(detail_id, "|", fixed = TRUE)[[1]]
  if (length(parts) < 2L) {
    return(list(id = detail_id, no = NA_character_))
  }

  list(
    id = parts[[1]],
    no = paste(parts[-1], collapse = "|")
  )
}

#' Build a results tibble from parsed JavaScript record lists
#' @noRd
build_results_tibble <- function(parsed_records) {
  return(tibble::tibble(
    thesis_no = result_record_column(parsed_records, "thesis_no"),
    title_original = result_record_column(parsed_records, "title_original"),
    title_translation = result_record_column(
      parsed_records,
      "title_translation"
    ),
    author = result_record_column(parsed_records, "author"),
    university = result_record_column(parsed_records, "university"),
    year = result_record_column(parsed_records, "year", NA_integer_),
    thesis_type_tr = result_record_column(parsed_records, "thesis_type_tr"),
    thesis_type_en = result_record_column(parsed_records, "thesis_type_en"),
    language_tr = result_record_column(parsed_records, "language_tr"),
    language_en = result_record_column(parsed_records, "language_en"),
    subject_tr = result_record_column(parsed_records, "subject_tr"),
    subject_en = result_record_column(parsed_records, "subject_en"),
    detail_id = result_record_column(parsed_records, "detail_id")
  ))
}

#' Extract one typed column from parsed search-result records
#' @noRd
result_record_column <- function(
  parsed_records,
  field_name,
  default = NA_character_
) {
  vapply(
    parsed_records,
    function(parsed_record) {
      coalesce_missing(parsed_record[[field_name]], default)
    },
    default
  )
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

#' Parse quoted JavaScript object string fields
#' @noRd
parse_js_quoted_fields <- function(text) {
  field_matches <- stringr::str_match_all(
    text,
    "\"([^\"]+)\"\\s*:\\s*\"((?:[^\"\\\\]|\\\\.)*)\""
  )[[1]]

  if (nrow(field_matches) == 0) {
    return(stats::setNames(character(), character()))
  }

  field_names <- field_matches[, 2]
  field_values <- normalize_js_field_values(field_matches[, 3])

  stats::setNames(field_values, field_names)
}

#' Extract modern result-card metadata from referenceData JavaScript
#' @noRd
extract_modern_reference_data <- function(html_text) {
  reference_match <- stringr::str_match(
    html_text,
    stringr::regex(
      "const\\s+referenceData\\s*=\\s*\\{([\\s\\S]*?)\\};",
      dotall = TRUE
    )
  )

  if (is.na(reference_match[1, 2])) {
    return(list())
  }

  entry_matches <- stringr::str_match_all(
    reference_match[1, 2],
    stringr::regex(
      "\"([0-9]+)\"\\s*:\\s*\\{\\s*\"meta\"\\s*:\\s*\\{([\\s\\S]*?)\\}\\s*\\}",
      dotall = TRUE
    )
  )[[1]]

  if (nrow(entry_matches) == 0) {
    return(list())
  }

  refs <- vector("list", nrow(entry_matches))
  names(refs) <- entry_matches[, 2]

  for (idx in seq_len(nrow(entry_matches))) {
    refs[[idx]] <- parse_js_quoted_fields(entry_matches[idx, 3])
  }

  refs
}

#' Parse redesigned result-card search results
#' @noRd
parse_modern_result_cards <- function(html) {
  cards <- rvest::html_elements(html, ".result-card")
  if (length(cards) == 0) {
    return(empty_results_tibble())
  }

  reference_data <- extract_modern_reference_data(as.character(html))
  parsed_records <- vector("list", length(cards))
  parsed_count <- 0L

  for (card in cards) {
    parsed_record <- parse_modern_result_card(card, reference_data)
    if (!is.null(parsed_record)) {
      parsed_count <- parsed_count + 1L
      parsed_records[[parsed_count]] <- parsed_record
    }
  }

  if (parsed_count == 0L) {
    return(empty_results_tibble())
  }

  build_results_tibble(parsed_records[seq_len(parsed_count)])
}

#' Parse one redesigned search result card
#' @noRd
parse_modern_result_card <- function(card, reference_data) {
  index <- rvest::html_attr(card, "data-index")
  kayit_no <- rvest::html_attr(card, "data-kayitno")
  tez_no <- rvest::html_attr(card, "data-tezno")
  meta <- reference_data[[index]]
  if (is.null(meta)) {
    meta <- stats::setNames(character(), character())
  }

  thesis_no <- extract_modern_thesis_no(card)
  if (is.na(thesis_no)) {
    return(NULL)
  }

  title <- parse_modern_card_title(card, meta)
  subject <- split_bilingual_subjects(normalize_modern_subject(
    get_doc_field(meta, "subject")
  ))
  language <- extract_stat_language(get_doc_field(meta, "lang"))
  thesis_type <- extract_stat_thesis_type(get_doc_field(meta, "type"))

  list(
    thesis_no = thesis_no,
    title_original = title$primary,
    title_translation = title$secondary,
    author = coalesce_missing(get_doc_field(meta, "author"), NA_character_),
    university = parse_modern_university(get_doc_field(meta, "yer")),
    year = coalesce_missing(
      suppressWarnings(as.integer(get_doc_field(meta, "year"))),
      NA_integer_
    ),
    thesis_type_tr = coalesce_missing(thesis_type$tr, NA_character_),
    thesis_type_en = coalesce_missing(thesis_type$en, NA_character_),
    language_tr = language$tr,
    language_en = language$en,
    subject_tr = subject$subject_tr,
    subject_en = subject$subject_en,
    detail_id = compose_detail_id(kayit_no, tez_no)
  )
}

#' Extract a numeric thesis number from a redesigned result card
#' @noRd
extract_modern_thesis_no <- function(card) {
  card_text <- rvest::html_text2(card)
  match <- stringr::str_match(card_text, "(?:Tez No|Thesis No):\\s*([0-9]+)")
  coalesce_missing(match[1, 2], NA_character_)
}

#' Extract title fields from a redesigned result card
#' @noRd
parse_modern_card_title <- function(card, meta) {
  card_title <- rvest::html_element(card, ".card-title") |>
    rvest::html_text2()
  title_original <- coalesce_missing(
    get_doc_field(meta, "title"),
    card_title
  )
  title_translation <- rvest::html_element(
    card,
    ".card-info[style*='italic']"
  ) |>
    rvest::html_text2()

  list(
    primary = coalesce_missing(clean_text(title_original), NA_character_),
    secondary = coalesce_missing(clean_text(title_translation), NA_character_)
  )
}

#' Extract the university name from modern location metadata
#' @noRd
parse_modern_university <- function(location) {
  location <- coalesce_missing(location, NA_character_)
  if (is.na(location)) {
    return(NA_character_)
  }

  parts <- strsplit(location, "/", fixed = TRUE)[[1]]
  coalesce_missing(clean_text(parts[[1]]), NA_character_)
}

#' Normalize compact semicolon-delimited modern subject metadata
#' @noRd
normalize_modern_subject <- function(subject) {
  subject <- coalesce_missing(subject, NA_character_)
  if (is.na(subject)) {
    return(NA_character_)
  }

  gsub(";", "; ", subject, fixed = TRUE)
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
    primary = coalesce_missing(title_primary, NA_character_),
    secondary = coalesce_missing(title_secondary, NA_character_)
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

  identity <- parse_js_doc_identity(userid_html)
  title <- parse_js_doc_title(parsed_fields)
  subject <- parse_js_doc_subject(parsed_fields)
  language <- parse_js_doc_language(parsed_fields)
  thesis_type <- extract_stat_thesis_type(get_doc_field(
    parsed_fields,
    "important"
  ))

  build_js_doc_record(
    identity,
    title,
    subject,
    language,
    thesis_type,
    parsed_fields
  )
}

#' Parse thesis and detail identifiers from a JavaScript userId field
#' @noRd
parse_js_doc_identity <- function(userid_html) {
  detail_match <- stringr::str_match(
    userid_html,
    "tezDetay\\('([^']+)'\\s*,\\s*'([^']+)'\\)"
  )

  detail_id <- if (!is.na(detail_match[1])) {
    compose_detail_id(detail_match[2], detail_match[3])
  } else {
    NA_character_
  }

  list(
    thesis_no = stringr::str_match(userid_html, ">([0-9]+)(?:</span>)?$")[2],
    detail_id = detail_id
  )
}

#' Parse original and translated title fields from JavaScript fields
#' @noRd
parse_js_doc_title <- function(parsed_fields) {
  title_raw <- get_doc_field(parsed_fields, "weight")
  parse_title_fields_fast(coalesce_missing(title_raw, ""))
}

#' Parse subject labels from JavaScript fields
#' @noRd
parse_js_doc_subject <- function(parsed_fields) {
  subject_raw <- coalesce_missing(
    get_doc_field(parsed_fields, "someDate"),
    NA_character_
  )
  split_bilingual_subjects(subject_raw)
}

#' Parse language labels from JavaScript fields
#' @noRd
parse_js_doc_language <- function(parsed_fields) {
  language_raw <- coalesce_missing(get_doc_field(parsed_fields, "height"), "")
  extract_stat_language(language_raw)
}

#' Build a normalized search-result record
#' @noRd
build_js_doc_record <- function(
  identity,
  title,
  subject,
  language,
  thesis_type,
  parsed_fields
) {
  list(
    thesis_no = coalesce_missing(identity$thesis_no, NA_character_),
    title_original = coalesce_missing(title$primary, NA_character_),
    title_translation = coalesce_missing(
      title$secondary,
      NA_character_
    ),
    author = coalesce_missing(
      get_doc_field(parsed_fields, "name"),
      NA_character_
    ),
    university = coalesce_missing(
      get_doc_field(parsed_fields, "uni"),
      NA_character_
    ),
    year = coalesce_missing(
      suppressWarnings(as.integer(get_doc_field(parsed_fields, "age"))),
      NA_integer_
    ),
    thesis_type_tr = coalesce_missing(thesis_type$tr, NA_character_),
    thesis_type_en = coalesce_missing(thesis_type$en, NA_character_),
    language_tr = language$tr,
    language_en = language$en,
    subject_tr = subject$subject_tr,
    subject_en = subject$subject_en,
    detail_id = identity$detail_id
  )
}
