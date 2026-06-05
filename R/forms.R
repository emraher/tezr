#' Coerce an optional form text field to the portal's empty-string sentinel
#' @noRd
form_text <- function(value, transform = identity) {
  if (is.null(value)) {
    return("")
  }

  transform(value)
}

#' Coerce an optional form integer field to the portal's zero sentinel
#' @noRd
form_int <- function(value) {
  if (is.null(value)) {
    return(0L)
  }

  as.integer(value)
}

#' Build basic search form data
#'
#' Creates the POST form data for basic search.
#' Field names are Turkish API parameters that cannot be changed.
#'
#' @noRd
build_basic_search_form <- function(
  keyword,
  search_field,
  thesis_type,
  access_type,
  year_start = NULL,
  year_end = NULL,
  language = NULL,
  group = "all",
  status = "approved"
) {
  coerced <- coerce_search_fields(year_start, year_end, language)
  year_start <- coerced$year_start
  year_end <- coerced$year_end
  language_id <- coerced$language_id

  return(list(
    # Search term (Tarama terimi)
    neden = keyword,
    # Search field (Aranacak Alan)
    nevi = search_field_codes[[search_field]],
    # Thesis type (Tez Türü)
    tur = thesis_type_codes[[thesis_type]],
    # Access type (İzin Durumu)
    izin = access_type_codes[[access_type]],
    # Start year
    yil1 = form_int(year_start),
    # End year
    yil2 = form_int(year_end),
    # Language (Dil)
    Dil = form_int(language_id),
    # Group
    EnstituGrubu = group_codes[[group]],
    # Status filter
    Durum = status_codes[[status]],
    # Operation type
    islem = 1L
  ))
}

#' Build advanced search form data
#'
#' Creates the POST form data for advanced keyword search.
#' Field names are Turkish API parameters that cannot be changed.
#'
#' @noRd
build_advanced_search_form <- function(
  keyword,
  search_field,
  thesis_type,
  access_type,
  year_start = NULL,
  year_end = NULL,
  language = NULL,
  group = "all",
  status = "approved",
  match_type = "exact",
  university = NULL,
  university_id = NULL,
  institute = NULL,
  institute_id = NULL
) {
  coerced <- coerce_search_fields(year_start, year_end, language)
  year_start <- coerced$year_start
  year_end <- coerced$year_end
  language_id <- coerced$language_id

  # Match the advanced search form fields from the web UI
  return(list(
    yil1 = form_int(year_start),
    yil2 = form_int(year_end),
    EnstituGrubu = group_codes[[group]],
    keyword = keyword,
    nevi = search_field_codes[[search_field]],
    tip = match_type_codes[[match_type]],
    Tur = thesis_type_codes[[thesis_type]],
    Dil = form_int(language_id),
    ops_field = "and",
    keyword1 = "",
    nevi2 = 1L,
    tip2 = 1L,
    izin = access_type_codes[[access_type]],
    Durum = status_codes[[status]],
    ops_field1 = "and",
    keyword2 = "",
    nevi3 = 1L,
    tip3 = 1L,
    uniad = form_text(university),
    Universite = form_int(university_id),
    ensad = form_text(institute),
    Enstitu = form_int(institute_id),
    islem = 4L,
    "-find" = "  Search  "
  ))
}

#' Build detailed search form data
#'
#' Creates the POST form data for detailed search.
#' Field names are Turkish API parameters that cannot be changed.
#'
#' @noRd
build_detailed_search_form <- function(
  thesis_no = NULL,
  title = NULL,
  author = NULL,
  supervisor = NULL,
  abstract = NULL,
  keyword = NULL,
  university = NULL,
  university_id = NULL,
  institute = NULL,
  institute_id = NULL,
  division = NULL,
  division_id = NULL,
  subject = NULL,
  subject_id = NULL,
  discipline = NULL,
  discipline_id = NULL,
  thesis_type = "all",
  year_start = NULL,
  year_end = NULL,
  language = NULL,
  access_type = "all",
  group = "all",
  status = "approved"
) {
  coerced <- coerce_search_fields(year_start, year_end, language)
  year_start <- coerced$year_start
  year_end <- coerced$year_end
  language_id <- coerced$language_id

  return(list(
    islem = 2L,
    "-find" = "  Bul  ",
    uniad = form_text(university),
    ensad = form_text(institute),
    abdad = form_text(division),
    TezNo = form_text(thesis_no, as.character),
    TezAd = form_text(title),
    AdSoyad = form_text(author),
    DanismanAdSoyad = clean_advisor_name(supervisor),
    Metin = form_text(abstract),
    Dizin = form_text(keyword),
    bilim = form_text(discipline),
    Konu = form_text(subject),
    EnstituGrubu = group_codes[[group]],
    Universite = form_int(university_id),
    Enstitu = form_int(institute_id),
    ABD = form_int(division_id),
    BilimDali = form_int(discipline_id),
    Bolum = 0L,
    Tur = thesis_type_codes[[thesis_type]],
    yil1 = form_int(year_start),
    yil2 = form_int(year_end),
    Dil = form_int(language_id),
    izin = access_type_codes[[access_type]],
    Durum = status_codes[[status]]
  ))
}
