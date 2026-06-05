#' Parse a thesis detail HTML page into a named list of metadata fields
#' @noRd
parse_detail_page <- function(html) {
  rows <- detail_horizontal_rows(html)
  if (has_horizontal_detail_rows(rows)) {
    return(parse_horizontal_detail_page(
      html,
      rows$header_row,
      rows$value_row
    ))
  }

  parse_vertical_detail_page(html)
}

#' Return horizontal table rows from a detail page
#' @noRd
detail_horizontal_rows <- function(html) {
  list(
    header_row = rvest::html_element(html, "tr.renkbas"),
    value_row = rvest::html_element(html, "tr.renkp")
  )
}

#' Return whether horizontal detail rows are present
#' @noRd
has_horizontal_detail_rows <- function(rows) {
  !is.na(rows$header_row) && !is.na(rows$value_row)
}

#' Parse a detail page that uses the horizontal table layout
#' @noRd
parse_horizontal_detail_page <- function(html, header_row, value_row) {
  columns <- horizontal_detail_columns(header_row, value_row)
  blocks <- horizontal_detail_blocks(columns)
  metadata_fields <- parse_horizontal_metadata_fields(
    html,
    blocks$metadata_text,
    blocks$stats_text
  )

  c(
    list(thesis_no = blocks$thesis_no),
    metadata_fields,
    list(
      access_status = extract_access_status(html, blocks$pdf_link),
      pdf_url = blocks$pdf_link
    )
  )
}

#' Map horizontal table headers to value cells
#' @noRd
horizontal_detail_columns <- function(header_row, value_row) {
  headers <- header_row |>
    rvest::html_elements("td") |>
    rvest::html_text() |>
    clean_text()
  col_map <- seq_along(headers)
  names(col_map) <- headers

  list(
    values = value_row |> rvest::html_elements("td"),
    col_map = col_map
  )
}

#' Return one horizontal detail cell by displayed label
#' @noRd
horizontal_detail_cell <- function(columns, label) {
  if (!label %in% names(columns$col_map)) {
    return(NULL)
  }

  columns$values[columns$col_map[label]]
}

#' Extract the top-level horizontal detail text blocks
#' @noRd
horizontal_detail_blocks <- function(columns) {
  thesis_cell <- horizontal_detail_cell(columns, "Tez No")
  metadata_cell <- horizontal_detail_cell(columns, "Tez K\u00FCnye")
  stats_cell <- horizontal_detail_cell(columns, "Durumu")

  list(
    thesis_no = detail_cell_text(thesis_cell),
    pdf_link = detail_pdf_link(horizontal_detail_cell(columns, "\u0130ndirme")),
    metadata_text = detail_cell_text(
      metadata_cell,
      default = "",
      clean = FALSE
    ),
    stats_text = detail_cell_text(
      stats_cell,
      default = "",
      collapse_whitespace = TRUE,
      clean = FALSE
    )
  )
}

#' Extract text from an optional detail cell
#' @noRd
detail_cell_text <- function(
  cell,
  default = NA_character_,
  collapse_whitespace = FALSE,
  clean = TRUE
) {
  if (is.null(cell)) {
    return(default)
  }

  text <- rvest::html_text(cell)
  if (collapse_whitespace) {
    text <- stringr::str_replace_all(text, "\\s+", " ")
  }

  if (clean) {
    return(clean_text(text))
  }

  text
}

#' Extract an absolute PDF link from an optional detail cell
#' @noRd
detail_pdf_link <- function(pdf_cell) {
  if (is.null(pdf_cell)) {
    return(NA_character_)
  }

  link_node <- rvest::html_element(pdf_cell, "a")
  if (is.na(link_node)) {
    return(NA_character_)
  }

  href <- rvest::html_attr(link_node, "href")
  if (is.na(href)) {
    return(NA_character_)
  }

  paste0(base_url, href)
}

#' Parse metadata from horizontal detail text blocks
#' @noRd
parse_horizontal_metadata_fields <- function(html, metadata_text, stats_text) {
  title_raw <- stringr::str_split(metadata_text, "Yazar\\s*:|Author\\s*:")[[
    1
  ]][1] |>
    clean_text()
  title_fields <- split_titles(title_raw)
  subjects <- split_bilingual_subjects(
    extract_labeled_value(metadata_text, "Konu|Subject")
  )
  index_keywords <- extract_keywords_from_index(
    extract_labeled_value(metadata_text, "Dizin|Index")
  )
  language <- extract_stat_language(stats_text)
  abstract_fields <- derive_abstract_fields_from_html(html, language$en)
  keyword_fields <- derive_keyword_fields_from_html(html)
  thesis_type <- extract_stat_thesis_type(stats_text)
  location <- parse_location_info(metadata_text)
  advisors <- split_advisors(extract_labeled_value(
    metadata_text,
    "Dan\u0131\u015Fman|Dani\u015Fman|Advisor"
  ))

  list(
    title_original = title_fields$primary,
    title_translation = title_fields$secondary,
    author = extract_labeled_value(metadata_text, "Yazar|Author"),
    advisor = advisors$advisor,
    co_advisor = advisors$co_advisor,
    university = location$university,
    institute = location$institute,
    division = location$division,
    year = extract_stat_year(stats_text),
    pages = extract_stat_pages(stats_text),
    thesis_type_tr = thesis_type$tr,
    thesis_type_en = thesis_type$en,
    language_tr = language$tr,
    language_en = language$en,
    subject_tr = subjects$subject_tr,
    subject_en = subjects$subject_en,
    abstract_original = abstract_fields$abstract_original,
    abstract_translation = abstract_fields$abstract_translation,
    keywords_tr = merge_keywords(keyword_fields$keywords_tr, index_keywords$tr),
    keywords_en = merge_keywords(keyword_fields$keywords_en, index_keywords$en)
  )
}

#' Derive abstract fields from detail-page abstract blocks
#' @noRd
derive_abstract_fields_from_html <- function(html, language_en) {
  td_text <- extract_td_text(html)
  derive_abstract_fields(
    td0_text = td_text$tr,
    td1_text = td_text$en,
    language_en = language_en
  )
}

#' Derive keyword fields from detail-page abstract blocks
#' @noRd
derive_keyword_fields_from_html <- function(html) {
  td_text <- extract_td_text(html)
  derive_keyword_fields(
    td0_text = td_text$tr,
    td1_text = td_text$en
  )
}

#' Parse a detail page that uses the older vertical field layout
#' @noRd
parse_vertical_detail_page <- function(html) {
  subjects <- split_bilingual_subjects(
    extract_first_detail_field(html, "Konu", "Subject")
  )
  index_keywords <- extract_keywords_from_index(
    extract_first_detail_field(html, "Dizin", "Index")
  )
  language <- extract_vertical_detail_language(html)
  abstract_fields <- derive_abstract_fields_from_html(html, language$en)
  thesis_type <- extract_vertical_detail_thesis_type(html)
  advisors <- split_advisors(extract_first_detail_field(
    html,
    "Dan\u0131\u015Fman",
    "Dani\u015Fman",
    "Advisor"
  ))

  build_vertical_detail_fields(
    html,
    subjects,
    index_keywords,
    language,
    abstract_fields,
    thesis_type,
    advisors
  )
}

#' Extract language labels from a vertical detail page
#' @noRd
extract_vertical_detail_language <- function(html) {
  language_raw <- extract_first_detail_field(html, "Dil", "Language")
  extract_stat_language(coalesce_missing(language_raw, ""))
}

#' Extract thesis type labels from a vertical detail page
#' @noRd
extract_vertical_detail_thesis_type <- function(html) {
  thesis_type_raw <- extract_first_detail_field(
    html,
    "T\u00FCr",
    "Tur",
    "Type"
  )
  extract_stat_thesis_type(coalesce_missing(thesis_type_raw, ""))
}

#' Build the normalized vertical detail field list
#' @noRd
build_vertical_detail_fields <- function(
  html,
  subjects,
  index_keywords,
  language,
  abstract_fields,
  thesis_type,
  advisors
) {
  keywords_tr <- merge_keywords(extract_keywords(html, "tr"), index_keywords$tr)
  keywords_en <- merge_keywords(extract_keywords(html, "en"), index_keywords$en)

  c(
    vertical_identity_fields(html),
    list(
      advisor = advisors$advisor,
      co_advisor = advisors$co_advisor
    ),
    vertical_location_fields(html),
    list(
      year = extract_first_detail_field(html, "Y\u0131l", "Yil", "Year"),
      thesis_type_tr = thesis_type$tr,
      thesis_type_en = thesis_type$en,
      pages = extract_vertical_detail_pages(html),
      language_tr = language$tr,
      language_en = language$en,
      subject_tr = subjects$subject_tr,
      subject_en = subjects$subject_en,
      abstract_original = abstract_fields$abstract_original,
      abstract_translation = abstract_fields$abstract_translation,
      keywords_tr = keywords_tr,
      keywords_en = keywords_en,
      access_status = extract_access_status(html),
      pdf_url = NA_character_
    )
  )
}

#' Extract identity fields from a vertical detail page
#' @noRd
vertical_identity_fields <- function(html) {
  list(
    thesis_no = extract_first_detail_field(html, "Tez No", "Thesis No"),
    title_original = extract_first_detail_field(
      html,
      "Tez Ad\u0131",
      "Tez Adi",
      "Thesis Title"
    ),
    title_translation = extract_first_detail_field(
      html,
      "Title",
      "\u0130ngilizce Tez Ad\u0131",
      "English Title"
    ),
    author = extract_first_detail_field(html, "Yazar", "Author")
  )
}

#' Extract location fields from a vertical detail page
#' @noRd
vertical_location_fields <- function(html) {
  list(
    university = extract_first_detail_field(
      html,
      "\u00DCniversite",
      "Universite",
      "University"
    ),
    institute = extract_first_detail_field(
      html,
      "Enstit\u00FC",
      "Enstitu",
      "Institute"
    ),
    division = extract_first_detail_field(
      html,
      "Anabilim Dal\u0131",
      "ABD",
      "Division"
    )
  )
}

#' Extract page-count field from a vertical detail page
#' @noRd
extract_vertical_detail_pages <- function(html) {
  extract_first_detail_field(
    html,
    "Sayfa Say\u0131s\u0131",
    "Sayfa",
    "Pages",
    "Page Count"
  )
}

#' Prefer the first non-missing, non-empty value
#' @noRd
coalesce_text <- function(...) {
  values <- list(...)

  for (value in values) {
    if (!is.null(value) && !is.na(value) && nchar(value) > 0) {
      return(value)
    }
  }

  NA_character_
}

#' Detect whether text appears to be Turkish
#' @noRd
looks_turkish_text <- function(text) {
  if (is.na(text) || nchar(text) == 0) {
    return(FALSE)
  }

  normalized <- stringi::stri_trans_tolower(text, locale = "tr")

  turkish_word_pattern <- paste0(
    "\\b(",
    "bu|tez|\u00e7al\u0131\u015fma|amac|ama\u00e7|sonu\u00e7|olarak|ile|",
    "i\u00e7in|\u00fczerine|anahtar",
    ")\\b"
  )

  grepl("[\u00e7\u011f\u0131\u00f6\u015f\u00fc]", normalized) ||
    grepl(
      turkish_word_pattern,
      normalized,
      perl = TRUE
    )
}

#' Detect whether text appears to be English
#' @noRd
looks_english_text <- function(text) {
  if (is.na(text) || nchar(text) == 0) {
    return(FALSE)
  }

  normalized <- tolower(text)

  english_word_pattern <- paste0(
    "\\b(",
    "this|thesis|study|abstract|chapter|analysis|results|using|findings|",
    "the|and|of|for",
    ")\\b"
  )

  grepl(
    english_word_pattern,
    normalized,
    perl = TRUE
  )
}

#' Detect whether text matches the script of the thesis language
#' @noRd
matches_original_language <- function(text, language_en) {
  if (
    is.na(text) ||
      nchar(text) == 0 ||
      is.na(language_en) ||
      nchar(language_en) == 0
  ) {
    return(FALSE)
  }

  switch(
    language_en,
    "Turkish" = looks_turkish_text(text),
    "English" = looks_english_text(text),
    "Arabic" = grepl("\\p{Arabic}", text, perl = TRUE),
    "Kirghiz" = grepl("\\p{Cyrillic}", text, perl = TRUE),
    "Russian" = grepl("\\p{Cyrillic}", text, perl = TRUE),
    "Kazakh" = grepl("\\p{Cyrillic}", text, perl = TRUE),
    FALSE
  )
}

#' Split one abstract block into abstract and keywords
#' @noRd
split_abstract_block <- function(text) {
  if (is.na(text) || nchar(text) == 0) {
    return(list(abstract = NA_character_, keywords = NA_character_))
  }

  parts <- stringr::str_split(
    text,
    stringr::regex(
      "Anahtar\\s+S[\u00f6o]zc[\u00fcu]kler\\s*:|Keywords\\s*:",
      ignore_case = TRUE
    ),
    n = 2
  )[[1]]

  abstract <- clean_text(parts[[1]])
  keywords <- if (length(parts) >= 2) clean_text(parts[[2]]) else NA_character_

  list(
    abstract = if (nchar(abstract) > 0) abstract else NA_character_,
    keywords = if (!is.na(keywords) && nchar(keywords) > 0) {
      keywords
    } else {
      NA_character_
    }
  )
}

#' Map abstract blocks to original/translation and Turkish/English views
#' @noRd
derive_abstract_fields <- function(td0_text, td1_text, language_en) {
  td0_parts <- split_abstract_block(td0_text)
  td1_parts <- split_abstract_block(td1_text)

  td0_abstract <- td0_parts$abstract
  td1_abstract <- td1_parts$abstract
  inferred_language <- infer_abstract_language(
    td0_abstract,
    td1_abstract,
    language_en
  )
  abstract_tr <- select_turkish_abstract(td0_abstract, td1_abstract)
  abstract_original <- select_original_abstract(
    td0_abstract,
    td1_abstract,
    inferred_language
  )
  abstract_translation <- select_translation_abstract(
    td0_abstract,
    td1_abstract,
    abstract_original
  )
  abstract_tr <- complete_turkish_abstract(
    abstract_tr,
    abstract_original,
    abstract_translation,
    inferred_language
  )

  list(
    abstract_tr = abstract_tr,
    abstract_original = abstract_original,
    abstract_translation = abstract_translation
  )
}

#' Infer the original language for paired abstract blocks
#' @noRd
infer_abstract_language <- function(td0_abstract, td1_abstract, language_en) {
  if (!is.na(language_en) && nchar(language_en) > 0) {
    return(language_en)
  }

  if (looks_turkish_text(td0_abstract) && looks_english_text(td1_abstract)) {
    return("Turkish")
  }

  if (looks_english_text(td0_abstract) && looks_turkish_text(td1_abstract)) {
    return("English")
  }

  language_en
}

#' Select the Turkish abstract from paired abstract blocks
#' @noRd
select_turkish_abstract <- function(td0_abstract, td1_abstract) {
  coalesce_text(
    if (looks_turkish_text(td0_abstract)) td0_abstract else NA_character_,
    if (looks_turkish_text(td1_abstract)) td1_abstract else NA_character_
  )
}

#' Select the original-language abstract from paired abstract blocks
#' @noRd
select_original_abstract <- function(
  td0_abstract,
  td1_abstract,
  inferred_language
) {
  if (identical(inferred_language, "Turkish")) {
    return(coalesce_text(td0_abstract, td1_abstract))
  }

  if (identical(inferred_language, "English")) {
    return(coalesce_text(
      english_original_candidate(td0_abstract),
      english_original_candidate(td1_abstract)
    ))
  }

  coalesce_text(
    original_language_candidate(td0_abstract, inferred_language),
    original_language_candidate(td1_abstract, inferred_language)
  )
}

#' Return an English original-language candidate
#' @noRd
english_original_candidate <- function(text) {
  if (looks_english_text(text) && !looks_turkish_text(text)) {
    return(text)
  }

  NA_character_
}

#' Return a candidate matching the inferred original language
#' @noRd
original_language_candidate <- function(text, inferred_language) {
  if (matches_original_language(text, inferred_language)) {
    return(text)
  }

  NA_character_
}

#' Select the non-original abstract as the translation
#' @noRd
select_translation_abstract <- function(
  td0_abstract,
  td1_abstract,
  abstract_original
) {
  remaining_td0 <- if (identical(td0_abstract, abstract_original)) {
    NA_character_
  } else {
    td0_abstract
  }
  remaining_td1 <- if (identical(td1_abstract, abstract_original)) {
    NA_character_
  } else {
    td1_abstract
  }

  coalesce_text(
    if (looks_turkish_text(remaining_td0)) remaining_td0 else NA_character_,
    if (looks_turkish_text(remaining_td1)) remaining_td1 else NA_character_,
    if (looks_english_text(remaining_td0)) remaining_td0 else NA_character_,
    if (looks_english_text(remaining_td1)) remaining_td1 else NA_character_,
    remaining_td0,
    remaining_td1
  )
}

#' Complete the Turkish abstract alias after original and translation mapping
#' @noRd
complete_turkish_abstract <- function(
  abstract_tr,
  abstract_original,
  abstract_translation,
  inferred_language
) {
  if (identical(inferred_language, "Turkish")) {
    return(coalesce_text(abstract_original, abstract_tr))
  }

  if (identical(inferred_language, "English")) {
    return(coalesce_text(
      if (looks_turkish_text(abstract_translation)) {
        abstract_translation
      } else {
        NA_character_
      },
      abstract_tr
    ))
  }

  abstract_tr
}

#' Map keyword blocks to Turkish and English slots
#' @noRd
derive_keyword_fields <- function(td0_text, td1_text) {
  td0_parts <- split_abstract_block(td0_text)
  td1_parts <- split_abstract_block(td1_text)

  keywords_tr <- coalesce_text(
    if (looks_turkish_text(td0_parts$keywords)) {
      td0_parts$keywords
    } else {
      NA_character_
    },
    if (looks_turkish_text(td1_parts$keywords)) {
      td1_parts$keywords
    } else {
      NA_character_
    }
  )

  keywords_en <- coalesce_text(
    if (looks_english_text(td0_parts$keywords)) {
      td0_parts$keywords
    } else {
      NA_character_
    },
    if (looks_english_text(td1_parts$keywords)) {
      td1_parts$keywords
    } else {
      NA_character_
    }
  )

  list(
    keywords_tr = keywords_tr,
    keywords_en = keywords_en
  )
}

#' Split advisor string into advisor and co-advisor
#'
#' YOK lists co-advisors on the same line, separated by semicolons.
#' Returns the first as advisor and any remaining as co_advisor
#' (semicolon-separated if multiple).
#'
#' @param advisor_text Character. Raw advisor string from metadata.
#' @return List with advisor and co_advisor components.
#' @noRd
split_advisors <- function(advisor_text) {
  if (is.na(advisor_text) || nchar(advisor_text) == 0) {
    return(list(advisor = NA_character_, co_advisor = NA_character_))
  }

  parts <- stringr::str_split(advisor_text, "\\s*;\\s*")[[1]]
  parts <- parts[nchar(stringr::str_squish(parts)) > 0]

  if (length(parts) == 0) {
    return(list(advisor = NA_character_, co_advisor = NA_character_))
  }

  advisor <- clean_text(parts[1])
  co_advisor <- if (length(parts) >= 2) {
    paste(purrr::map_chr(parts[-1], clean_text), collapse = "; ")
  } else {
    NA_character_
  }

  return(list(advisor = advisor, co_advisor = co_advisor))
}

#' Split a bilingual title on "/" into primary and secondary parts
#' @noRd
split_titles <- function(title_text) {
  if (is.na(title_text) || nchar(title_text) == 0) {
    return(list(primary = NA_character_, secondary = NA_character_))
  }

  normalized_title <- clean_text(title_text)
  normalized_title <- sub("\\s*/\\s*$", "", normalized_title)
  parts <- stringr::str_split(title_text, "\\s*/\\s*")[[1]]

  if (length(parts) >= 2) {
    primary_title <- clean_text(parts[1])
    secondary_title <- clean_text(parts[2])

    # Bilingual titles typically start a new title after "/".
    # If the post-slash fragment begins with lowercase text, it is more
    # likely to be part of the same title than a translated title.
    if (grepl("^[[:lower:]]", secondary_title)) {
      return(list(
        primary = normalized_title,
        secondary = NA_character_
      ))
    }

    return(list(primary = primary_title, secondary = secondary_title))
  }

  return(list(primary = normalized_title, secondary = NA_character_))
}

#' Extract labeled value from metadata text
#'
#' Extracts values for fields like "Yazar: Name" or "Author: Name"
#' Supports both Turkish and English field labels
#'
#' @noRd
extract_labeled_value <- function(text, label_pattern) {
  # Stop pattern includes both Turkish and English labels
  stop_labels <- paste0(
    "Yazar|Author|Dan\u0131\u015Fman|Dani\u015Fman|Advisor|",
    "Yer Bilgisi|Location|Konu|Subject|Dizin|Index"
  )

  pattern <- paste0(
    "(",
    label_pattern,
    ")\\s*:\\s*(.*?)(?=\\s*(?:",
    stop_labels,
    "|$))"
  )
  match <- stringr::str_match(text, pattern)
  if (!is.na(match[1])) {
    return(clean_text(match[3]))
  }
  return(NA_character_)
}

#' Parse location information (Yer Bilgisi)
#'
#' Extracts university, institute, and division from location field
#' Supports both Turkish "Yer Bilgisi" and English "Location"
#'
#' @noRd
parse_location_info <- function(text) {
  val <- extract_labeled_value(text, "Yer Bilgisi|Location")
  if (is.na(val)) {
    return(list(university = NA, institute = NA, division = NA))
  }

  parts <- stringr::str_split(val, "\\s*/\\s*")[[1]]

  return(list(
    university = if (length(parts) >= 1) {
      clean_text(parts[1])
    } else {
      NA_character_
    },
    institute = if (length(parts) >= 2) clean_text(parts[2]) else NA_character_,
    division = if (length(parts) >= 3) clean_text(parts[3]) else NA_character_
  ))
}

#' Extract a four-digit year from the status column text
#' @noRd
extract_stat_year <- function(text) {
  match <- stringr::str_extract(text, "\\b(19|20)\\d{2}\\b")
  if (!is.na(match)) {
    return(match)
  }
  return(NA_character_)
}

#' Extract page count from status column text (e.g. "153 s.")
#' @noRd
extract_stat_pages <- function(text) {
  match <- stringr::str_match(text, "(\\d+)\\s*s\\.")
  if (!is.na(match[1])) {
    return(match[2])
  }
  return(NA_character_)
}

#' Generic lookup helper for Turkish/English bilingual data
#'
#' Extracts values from reference data with Turkish character normalization.
#'
#' @param cache_key Optional character key for reusing precomputed lookup
#'   metadata across repeated calls.
#' @param text Text to search in
#' @param reference_data Data frame with label_tr and label_en columns
#' @return List with tr and en components
#' @noRd
bilingual_lookup_cache <- new.env(parent = emptyenv())
bilingual_lookup_result_cache <- new.env(parent = emptyenv())

#' Build cache key for bilingual lookup result values
#' @noRd
build_bilingual_lookup_result_key <- function(cache_key, text) {
  return(paste0(cache_key, "::", text))
}

#' Drop cached bilingual lookup result values for one metadata cache key
#' @noRd
clear_bilingual_lookup_result_cache <- function(cache_key) {
  if (
    is.null(cache_key) ||
      length(cache_key) != 1 ||
      is.na(cache_key) ||
      nchar(cache_key) == 0
  ) {
    return(invisible(NULL))
  }

  cache_prefix <- paste0(cache_key, "::")
  existing_cache_keys <- ls(
    envir = bilingual_lookup_result_cache,
    all.names = TRUE
  )
  matching_cache_keys <- existing_cache_keys[startsWith(
    existing_cache_keys,
    cache_prefix
  )]

  if (length(matching_cache_keys) > 0) {
    rm(list = matching_cache_keys, envir = bilingual_lookup_result_cache)
  }

  return(invisible(NULL))
}

#' Build reusable regex and normalized labels for bilingual lookup
#' @noRd
build_bilingual_lookup_metadata <- function(reference_data) {
  label_tr <- reference_data$label_tr
  label_en <- reference_data$label_en
  label_tr[is.na(label_tr)] <- ""
  label_en[is.na(label_en)] <- NA_character_

  pattern <- label_tr |>
    stringr::str_replace_all(stringr::fixed("\u0130"), "[\u0130I]") |>
    stringr::str_replace_all(stringr::fixed("\u0131"), "[\u0131i]") |>
    stringr::str_replace_all(stringr::fixed("\u015F"), "[\u015Fs]") |>
    stringr::str_replace_all(stringr::fixed("\u011F"), "[\u011Fg]") |>
    stringr::str_replace_all(stringr::fixed("\u00FC"), "[\u00FCu]") |>
    stringr::str_replace_all(stringr::fixed("\u00F6"), "[\u00F6o]") |>
    stringr::str_replace_all(stringr::fixed("\u00E7"), "[\u00E7c]") |>
    paste(collapse = "|")

  return(list(
    label_tr = reference_data$label_tr,
    label_en = reference_data$label_en,
    label_tr_lower = stringr::str_to_lower(label_tr),
    pattern = stringr::regex(pattern, ignore_case = TRUE)
  ))
}

#' Retrieve cached bilingual lookup metadata or build it on demand
#' @noRd
valid_bilingual_lookup_cache_key <- function(cache_key) {
  !is.null(cache_key) &&
    length(cache_key) == 1 &&
    !is.na(cache_key) &&
    nchar(cache_key) > 0
}

#' Return whether cached lookup metadata matches the reference data
#' @noRd
cached_bilingual_lookup_matches <- function(cached_metadata, reference_data) {
  identical(cached_metadata$label_tr, reference_data$label_tr) &&
    identical(cached_metadata$label_en, reference_data$label_en)
}

#' Retrieve cached bilingual lookup metadata or build it on demand
#' @noRd
get_bilingual_lookup_metadata <- function(reference_data, cache_key = NULL) {
  if (!valid_bilingual_lookup_cache_key(cache_key)) {
    return(build_bilingual_lookup_metadata(reference_data))
  }

  if (exists(cache_key, envir = bilingual_lookup_cache, inherits = FALSE)) {
    cached_metadata <- bilingual_lookup_cache[[cache_key]]
    if (cached_bilingual_lookup_matches(cached_metadata, reference_data)) {
      return(cached_metadata)
    }
  }

  clear_bilingual_lookup_result_cache(cache_key)
  bilingual_lookup_cache[[cache_key]] <- build_bilingual_lookup_metadata(
    reference_data
  )

  return(bilingual_lookup_cache[[cache_key]])
}

extract_bilingual_lookup <- function(text, reference_data, cache_key = NULL) {
  if (is.na(text) || nchar(text) == 0) {
    return(list(tr = NA_character_, en = NA_character_))
  }

  result_cache <- get_bilingual_lookup_result_cache(cache_key, text)
  if (!is.null(result_cache$result)) {
    return(result_cache$result)
  }

  lookup_metadata <- get_bilingual_lookup_metadata(
    reference_data,
    cache_key = cache_key
  )
  lookup_result <- match_bilingual_lookup_result(text, lookup_metadata)

  set_bilingual_lookup_result_cache(result_cache$key, lookup_result)
  return(lookup_result)
}

#' Return a cached bilingual lookup result, if present
#' @noRd
get_bilingual_lookup_result_cache <- function(cache_key, text) {
  if (!valid_bilingual_lookup_cache_key(cache_key)) {
    return(list(key = NULL, result = NULL))
  }

  result_cache_key <- build_bilingual_lookup_result_key(cache_key, text)
  if (
    exists(
      result_cache_key,
      envir = bilingual_lookup_result_cache,
      inherits = FALSE
    )
  ) {
    return(list(
      key = result_cache_key,
      result = bilingual_lookup_result_cache[[result_cache_key]]
    ))
  }

  list(key = result_cache_key, result = NULL)
}

#' Store a bilingual lookup result if result caching is enabled
#' @noRd
set_bilingual_lookup_result_cache <- function(result_cache_key, lookup_result) {
  if (!is.null(result_cache_key)) {
    bilingual_lookup_result_cache[[result_cache_key]] <- lookup_result
  }

  invisible(NULL)
}

#' Match text to a bilingual lookup label
#' @noRd
match_bilingual_lookup_result <- function(text, lookup_metadata) {
  match <- stringr::str_extract(text, lookup_metadata$pattern)

  if (is.na(match)) {
    return(list(tr = NA_character_, en = NA_character_))
  }

  match_indices <- which(stringr::str_detect(
    lookup_metadata$label_tr_lower,
    stringr::fixed(stringr::str_to_lower(match))
  ))

  if (length(match_indices) == 0) {
    return(list(tr = NA_character_, en = NA_character_))
  }

  first_index <- match_indices[[1]]
  list(
    tr = lookup_metadata$label_tr[[first_index]],
    en = lookup_metadata$label_en[[first_index]]
  )
}

#' Match thesis type label against known types and return TR/EN pair
#' @noRd
extract_stat_thesis_type <- function(text) {
  return(extract_bilingual_lookup(
    text,
    thesis_types,
    cache_key = "thesis_types"
  ))
}

#' Match language label against known languages and return TR/EN pair
#' @noRd
extract_stat_language <- function(text) {
  return(extract_bilingual_lookup(text, languages, cache_key = "languages"))
}

#' Extract a labeled value from the vertical detail table by field name
#' @noRd
extract_detail_field <- function(html, label) {
  xpath_label <- sprintf("//td[contains(text(), '%s')]", label)
  label_node <- rvest::html_element(html, xpath = xpath_label)

  if (!is.na(label_node)) {
    sibling1 <- rvest::html_element(
      label_node,
      xpath = "following-sibling::td[1]"
    )
    val1 <- if (!is.na(sibling1)) clean_text(rvest::html_text(sibling1)) else ""

    if (grepl("^[:.-]$", val1) || nchar(val1) == 0) {
      sibling2 <- rvest::html_element(
        label_node,
        xpath = "following-sibling::td[2]"
      )
      val2 <- if (!is.na(sibling2)) {
        clean_text(rvest::html_text(sibling2))
      } else {
        ""
      }
      if (nchar(val2) > 0) {
        return(val2)
      }
    } else {
      if (nchar(val1) > 0) {
        return(val1)
      }
    }
  }

  xpath2 <- sprintf(
    paste0(
      "//*[contains(@class, 'label') and contains(text(), '%s')]",
      "/following-sibling::*[1]"
    ),
    label
  )
  node2 <- rvest::html_element(html, xpath = xpath2)

  if (!is.na(node2)) {
    value <- clean_text(rvest::html_text(node2))
    if (nchar(value) > 0) {
      return(value)
    }
  }

  return(NA_character_)
}

#' Extract the first available vertical detail-table field
#' @noRd
extract_first_detail_field <- function(html, ...) {
  labels <- c(...)

  for (label in labels) {
    value <- extract_detail_field(html, label)
    if (!is.na(value) && nchar(value) > 0) {
      return(value)
    }
  }

  NA_character_
}

#' Determine whether a thesis is open-access or restricted
#' @noRd
extract_access_status <- function(html, pdf_link = NULL) {
  if (!is.null(pdf_link) && !is.na(pdf_link) && nchar(pdf_link) > 0) {
    return("open")
  }

  page_text <- rvest::html_text(html)
  if (
    stringr::str_detect(
      page_text,
      stringr::regex(
        "\u0130zinsiz|Restricted|Eri\u015Fime Kapal\u0131",
        ignore_case = TRUE
      )
    )
  ) {
    return("restricted")
  }

  return(NA_character_)
}

#' Extract keywords from the abstract text block by splitting on the label
#' @noRd
extract_keywords <- function(html, lang, td_text = NULL) {
  if (is.null(td_text)) {
    td_text <- extract_td_text(html)
  }
  full_text <- td_text[[lang]]
  parsed_block <- split_abstract_block(full_text)

  if (!is.na(parsed_block$keywords) && nchar(parsed_block$keywords) > 0) {
    return(parsed_block$keywords)
  }

  return(NA_character_)
}

#' Extract the raw text from td0 (Turkish) and td1 (English) abstract blocks
#' @noRd
extract_td_text <- function(html) {
  tr_node <- rvest::html_element(html, "#td0")
  en_node <- rvest::html_element(html, "#td1")

  list(
    tr = if (!is.na(tr_node)) {
      clean_text(rvest::html_text(tr_node))
    } else {
      NA_character_
    },
    en = if (!is.na(en_node)) {
      clean_text(rvest::html_text(en_node))
    } else {
      NA_character_
    }
  )
}

#' Extract keywords from index field
#'
#' The index field contains entries like "TurkishTerm=EnglishTerm;
#' Term2=Value2". This function splits on "=" and returns separate Turkish and
#' English keywords.
#'
#' @param index_raw Character. Raw index string from the metadata
#' @return List with tr and en components containing semicolon-separated
#'   keywords
#' @noRd
extract_keywords_from_index <- function(index_raw) {
  if (is.na(index_raw) || nchar(index_raw) == 0) {
    return(list(tr = NA_character_, en = NA_character_))
  }

  parsed <- parse_bilingual_entries(index_raw)
  keep <- !is.na(parsed$en) & nchar(parsed$en) > 0

  keywords_tr <- parsed$tr[keep]
  keywords_en <- parsed$en[keep]

  result_tr <- if (length(keywords_tr) == 0) {
    NA_character_
  } else {
    paste(keywords_tr, collapse = "; ")
  }

  result_en <- if (length(keywords_en) == 0) {
    NA_character_
  } else {
    paste(keywords_en, collapse = "; ")
  }

  return(list(tr = result_tr, en = result_en))
}

#' Merge index keywords with existing keywords
#'
#' Combines keywords from the index field with existing keywords,
#' avoiding duplicates.
#'
#' @param existing_keywords Character. Existing keywords string
#' @param index_keywords Character. Keywords extracted from index
#' @return Character. Combined keywords string
#' @noRd
merge_keywords <- function(existing_keywords, index_keywords) {
  # Collect all keywords
  all_keywords <- character(0)

  # Add existing keywords
  if (!is.na(existing_keywords) && nchar(existing_keywords) > 0) {
    existing <- stringr::str_split(existing_keywords, "[;,]")[[1]] |>
      purrr::map_chr(clean_text) |>
      purrr::discard(~ nchar(.x) == 0)
    all_keywords <- c(all_keywords, existing)
  }

  # Add index keywords
  if (!is.na(index_keywords) && nchar(index_keywords) > 0) {
    index_kw <- stringr::str_split(index_keywords, stringr::fixed(";"))[[1]] |>
      purrr::map_chr(clean_text) |>
      purrr::discard(~ nchar(.x) == 0)
    all_keywords <- c(all_keywords, index_kw)
  }

  # Remove duplicates (case-insensitive)
  if (length(all_keywords) == 0) {
    return(NA_character_)
  }

  unique_keywords <- all_keywords[!duplicated(tolower(all_keywords))]

  return(paste(unique_keywords, collapse = "; "))
}

#' Extract total count from results or detail pages
#'
#' Handles both "X kayit bulundu" (search results) and
#' "Tarama sonucunda X kayit" (detail pages) patterns.
#'
#' @noRd
extract_total_count <- function(html) {
  page_text <- rvest::html_text(html)

  # Try "X kayit bulundu" pattern first (search results)
  match <- stringr::str_match(page_text, "([0-9,]+)\\s+kay\u0131t bulundu")

  # Try "Tarama sonucunda X kayit" pattern (detail pages)
  if (is.na(match[1])) {
    match <- stringr::str_match(page_text, "Tarama sonucunda\\s+(\\d+)\\s+kay")
  }

  if (is.na(match[1])) {
    return(0L)
  }

  count_str <- stringr::str_remove_all(match[2], stringr::fixed(","))
  count <- suppressWarnings(as.integer(count_str))

  if (is.na(count)) {
    return(0L)
  }

  return(count)
}
