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
    neden = keyword, # Search term (Tarama terimi)
    nevi = search_field_codes[[search_field]], # Search field (Aranacak Alan)
    tur = thesis_type_codes[[thesis_type]], # Thesis type (Tez Türü)
    izin = access_type_codes[[access_type]], # Access type (İzin Durumu)
    yil1 = if (!is.null(year_start)) as.integer(year_start) else 0L, # Start year
    yil2 = if (!is.null(year_end)) as.integer(year_end) else 0L, # End year
    Dil = if (!is.null(language_id)) language_id else 0L, # Language (Dil)
    EnstituGrubu = group_codes[[group]], # Group
    Durum = status_codes[[status]], # Status filter
    islem = 1L # Operation type
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
    yil1 = if (!is.null(year_start)) as.integer(year_start) else 0L,
    yil2 = if (!is.null(year_end)) as.integer(year_end) else 0L,
    EnstituGrubu = group_codes[[group]],
    keyword = keyword,
    nevi = search_field_codes[[search_field]],
    tip = match_type_codes[[match_type]],
    Tur = thesis_type_codes[[thesis_type]],
    Dil = if (!is.null(language_id)) language_id else 0L,
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
    uniad = if (!is.null(university)) university else "",
    Universite = if (!is.null(university_id)) as.integer(university_id) else 0L,
    ensad = if (!is.null(institute)) institute else "",
    Enstitu = if (!is.null(institute_id)) as.integer(institute_id) else 0L,
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
    islem = 2L, # Operation type: detailed search
    "-find" = "  Bul  ", # Submit button text
    uniad = if (!is.null(university)) university else "", # University name (text search)
    ensad = if (!is.null(institute)) institute else "", # Institute name (text search)
    abdad = if (!is.null(division)) division else "", # Division name (Anabilim Dalı)
    TezNo = if (!is.null(thesis_no)) as.character(thesis_no) else "", # Thesis number
    TezAd = if (!is.null(title)) title else "", # Title (Tez Adı)
    AdSoyad = if (!is.null(author)) author else "", # Author name (Yazar)
    DanismanAdSoyad = clean_advisor_name(supervisor), # Supervisor name (Danışman)
    Metin = if (!is.null(abstract)) abstract else "", # Abstract text (Özet - Metin)
    Dizin = if (!is.null(keyword)) keyword else "", # Keyword text (Dizin)
    bilim = if (!is.null(discipline)) discipline else "", # Discipline name (Bilim Dalı)
    Konu = if (!is.null(subject)) subject else "", # Subject (Konu)
    EnstituGrubu = group_codes[[group]], # Group
    Universite = if (!is.null(university_id)) as.integer(university_id) else 0L, # University ID
    Enstitu = if (!is.null(institute_id)) as.integer(institute_id) else 0L, # Institute ID (Enstitü)
    ABD = if (!is.null(division_id)) as.integer(division_id) else 0L, # Division ID (Anabilim Dalı)
    BilimDali = if (!is.null(discipline_id)) as.integer(discipline_id) else 0L, # Discipline ID
    Bolum = 0L, # Section (unused)
    Tur = thesis_type_codes[[thesis_type]], # Thesis type (Tez Türü)
    yil1 = if (!is.null(year_start)) as.integer(year_start) else 0L, # Start year
    yil2 = if (!is.null(year_end)) as.integer(year_end) else 0L, # End year
    Dil = if (!is.null(language_id)) language_id else 0L, # Language (Dil)
    izin = access_type_codes[[access_type]], # Access type (İzin Durumu)
    Durum = status_codes[[status]] # Status filter
  ))
}
