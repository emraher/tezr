#' Search field codes
#' Maps English field names to API codes (Aranacak Alan)
#' @noRd
search_field_codes <- list(
  title = 1L, # Tez Adı (Thesis title)
  author = 2L, # Yazar (Author)
  supervisor = 3L, # Danışman (Supervisor)
  subject = 4L, # Konu (Subject)
  index = 5L, # Dizin (Index)
  abstract = 6L, # Özet (Abstract)
  all = 7L, # Tümü (All)
  thesis_no = 8L # Tez No (Thesis number)
)

#' Thesis type codes
#' Maps English names to API codes (Tez Türü)
#' @noRd
thesis_type_codes <- list(
  all = 0L,
  masters = 1L, # Yüksek Lisans (Master's degree)
  phd = 2L, # Doktora (Doctorate)
  medical_specialty = 3L, # Tıpta Uzmanlık (Medical specialty)
  arts = 4L, # Sanatta Yeterlik (Artistic competency)
  dentistry = 5L, # Diş Hekimliği Uzmanlık (Dental specialty)
  medical_sub = 6L, # Tıpta Yan Dal Uzmanlık (Medical sub-specialty)
  pharmacy = 7L # Eczacılıkta Uzmanlık (Pharmacy specialty)
)

#' Access status codes
#' Maps English names to API codes (İzin Durumu)
#' @noRd
access_type_codes <- list(
  all = 0L, # Tümü (All)
  open = 1L, # İzinli (Authorized/Open access)
  restricted = 2L # İzinsiz (Restricted)
)

#' Group codes
#' Maps English names to API codes (Enstitü Grubu)
#' @noRd
group_codes <- list(
  all = "", # Tümü (All)
  science = "F", # Fen Bilimleri (Science)
  social = "S", # Sosyal Bilimler (Social Sciences)
  medical = "T" # Tıp ve Sağlık Bilimleri (Medicine & Health)
)

#' Match type codes
#' Maps English names to API codes (tip)
#' @noRd
match_type_codes <- list(
  exact = 1L, # Sadece yazılan şekilde (Exact match only)
  contains = 2L # İçinde geçsin (Contains/substring match)
)

#' Status codes
#' Maps English names to API codes (Durum)
#' @noRd
status_codes <- list(
  all = 0L, # Tümü (All)
  in_preparation = 1L, # Hazırlanıyor (In preparation)
  approved = 3L # Onaylandı (Approved)
)
