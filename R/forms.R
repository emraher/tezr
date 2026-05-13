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
  status = "approved"
) {
  coerced <- coerce_search_fields(year_start, year_end, language)
  year_start <- coerced$year_start
  year_end <- coerced$year_end
  language_id <- coerced$language_id

  return(build_keyword_search_form(
    keyword = keyword,
    search_field = search_field,
    thesis_type = thesis_type,
    access_type = access_type,
    year_start = year_start,
    year_end = year_end,
    language_id = language_id,
    status = status,
    match_type = "contains"
  ))
}

#' Build 2026 keyword search form data
#' @noRd
build_keyword_search_form <- function(
  keyword,
  search_field,
  thesis_type,
  access_type,
  year_start = NULL,
  year_end = NULL,
  language_id = NULL,
  status = "approved",
  match_type = "contains",
  keyword_2 = "",
  keyword_3 = "",
  operator_1 = "and",
  operator_2 = "and"
) {
  list(
    keyword = keyword,
    keyword1 = keyword_2,
    keyword2 = keyword_3,
    ops_field = operator_1,
    ops_field1 = operator_2,
    nevi = search_field_codes[[search_field]],
    tip = match_type_codes[[match_type]],
    Tur = thesis_type_codes[[thesis_type]],
    Dil = if (!is.null(language_id)) language_id else 0L,
    izin = access_type_codes[[access_type]],
    Durum = status_codes[[status]],
    yil1 = if (!is.null(year_start)) as.integer(year_start) else 0L,
    yil2 = if (!is.null(year_end)) as.integer(year_end) else 0L,
    islem = 4L,
    "-find" = "  Bul"
  )
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
  university_id = NULL
) {
  coerced <- coerce_search_fields(year_start, year_end, language)
  year_start <- coerced$year_start
  year_end <- coerced$year_end
  language_id <- coerced$language_id

  form <- build_keyword_search_form(
    keyword = keyword,
    search_field = search_field,
    thesis_type = thesis_type,
    access_type = access_type,
    year_start = year_start,
    year_end = year_end,
    language_id = language_id,
    status = status,
    match_type = match_type
  )

  if (!identical(group, "all")) {
    form$EnstituGrubu <- group_codes[[group]]
  }

  if (!is.null(university_id)) {
    form$Universite <- as.integer(university_id)
    form$source <- "TR"
  }

  return(form)
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

  form <- list(
    uniad = if (!is.null(university)) university else "", # University name (text search)
    Universite = if (!is.null(university_id)) as.integer(university_id) else "", # University ID
    uni_yoksis_id = "", # Required by the redesigned detailed form
    source = "TR", # YOK source selector
    ensad = if (!is.null(institute)) institute else "", # Institute name (text search)
    Enstitu = if (!is.null(institute_id)) as.integer(institute_id) else 0L, # Institute ID (Enstitü)
    abdad = if (!is.null(division)) division else "", # Division name (Anabilim Dalı)
    ABD = if (!is.null(division_id)) as.integer(division_id) else 0L, # Division ID (Anabilim Dalı)
    Konu = if (!is.null(subject)) subject else "", # Subject (Konu)
    Tur = thesis_type_codes[[thesis_type]], # Thesis type (Tez Türü)
    yil1 = if (!is.null(year_start)) as.integer(year_start) else 0L, # Start year
    yil2 = if (!is.null(year_end)) as.integer(year_end) else 0L, # End year
    izin = access_type_codes[[access_type]], # Access type (İzin Durumu)
    Durum = status_codes[[status]], # Status filter
    TezAd = if (!is.null(title)) title else "", # Title (Tez Adı)
    Dil = if (!is.null(language_id)) language_id else 0L, # Language (Dil)
    AdSoyad = if (!is.null(author)) author else "", # Author name (Yazar)
    DanismanAdSoyad = clean_advisor_name(supervisor), # Supervisor name (Danışman)
    Dizin = if (!is.null(keyword)) keyword else "", # Keyword text (Dizin)
    TezNo = if (!is.null(thesis_no)) as.character(thesis_no) else "", # Thesis number
    islem = 2L, # Operation type: detailed search
    Bolum = 0L, # Section (unused)
    "-find" = "Search", # Submit button text
    Metin = if (!is.null(abstract)) abstract else "", # Abstract text (Özet - Metin)
    bilim = if (!is.null(discipline)) discipline else "", # Discipline name (Bilim Dalı)
    EnstituGrubu = group_codes[[group]], # Group
    BilimDali = if (!is.null(discipline_id)) as.integer(discipline_id) else 0L # Discipline ID
  )

  if (!is.null(institute) || !is.null(institute_id)) {
    form$selected_institute <- "on"
  }

  form
}
