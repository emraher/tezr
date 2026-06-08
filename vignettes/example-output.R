tezr_example_search_results <- function() {
  tibble::tibble(
    thesis_no = c(
      "967755",
      "975988",
      "955779",
      "974976",
      "960162",
      "946580",
      "928189",
      "968778"
    ),
    title_original = c(
      "Parasal aktarım mekanizması çerçevesinde özel sektöre kredi",
      "1980 sonrası Türkiye'de ayçiçeği piyasası",
      "Doğrudan yabancı yatırımlar ve ekonomik büyüme",
      "Enerji kullanımı ile G-20 ülkelerinde büyüme ilişkisi",
      "Finansal liberalleşme sürecinde kırılganlık",
      "Azerbaycan'ın ekolojik ayak izi üzerine bir analiz",
      "The impact of ESG scores on firm performance",
      "Ekonomik politika belirsizliğinin etkileri"
    ),
    title_translation = c(
      "Domestic credit to private sector and monetary transmission",
      "Changes in sunflower markets in Turkiye after 1980",
      "Foreign direct investment and economic growth",
      "Panel data analysis of energy use and growth",
      "Fragility during financial liberalization",
      "An analysis of Azerbaijan's ecological footprint",
      "ESG skorlarının firma performansına etkisi",
      "The effects of economic policy uncertainty"
    ),
    author = c(
      "PERIHAN EZGI BALLI",
      "CENAP ALAYBEYI",
      "SECIL ALMIS",
      "RUMEYSA YILMAZ",
      "SAMI KAYA",
      "ADIL MAMMADOV",
      "OGUZ DEMIR",
      "HARUN AK"
    ),
    university = c(
      "Bandirma Onyedi Eylul Universitesi",
      "Harran Universitesi",
      "Ege Universitesi",
      "Sivas Cumhuriyet Universitesi",
      "Istanbul Universitesi",
      "Kutahya Dumlupinar Universitesi",
      "Istanbul Teknik Universitesi",
      "Marmara Universitesi"
    ),
    year = c(2025L, 2025L, 2025L, 2024L, 2023L, 2022L, 2021L, 2020L),
    thesis_type_tr = c(
      "Doktora",
      "Yuksek Lisans",
      "Yuksek Lisans",
      "Yuksek Lisans",
      "Doktora",
      "Doktora",
      "Yuksek Lisans",
      "Doktora"
    ),
    thesis_type_en = c(
      "Doctorate",
      "Master",
      "Master",
      "Master",
      "Doctorate",
      "Doctorate",
      "Master",
      "Doctorate"
    ),
    language_tr = c(
      "Turkce",
      "Turkce",
      "Turkce",
      "Turkce",
      "Turkce",
      "Ingilizce",
      "Ingilizce",
      "Turkce"
    ),
    language_en = c(
      "Turkish",
      "Turkish",
      "Turkish",
      "Turkish",
      "Turkish",
      "English",
      "English",
      "Turkish"
    ),
    subject_tr = c(
      "Ekonometri; Ekonomi",
      "Ekonometri; Ekonomi; Ziraat",
      "Ekonomi",
      "Enerji; Ekonomi",
      "Ekonometri; Ekonomi",
      "Cevre Muhendisligi; Ekonomi",
      "Isletme",
      "Ekonomi"
    ),
    subject_en = c(
      "Econometrics; Economics",
      "Econometrics; Economics; Agriculture",
      "Economics",
      "Energy; Economics",
      "Econometrics; Economics",
      "Environmental Engineering; Economics",
      "Business Administration",
      "Economics"
    ),
    detail_id = c(
      "TCKf4ksTOVsOBqUcPYMKWQ",
      "LypZzbdoWcG0f3c62wverw",
      "xSXEsNoVFhD23qIBp8YBHw",
      "IJWqjV9Fqj1V3uAc01qNVw",
      "h8m0hAyVIXV5l0pzt6sd8A",
      "MglYaqsmdTUQsV07U0Kltg",
      "g4dEDR2n1STfJ1RjtBjuCg",
      "bDt6ffmxN2WE9DvSXge4jQ"
    )
  )
}

tezr_example_topic_results <- function(
  title_original,
  title_translation,
  subject_tr,
  subject_en
) {
  tezr_example_search_results() |>
    dplyr::mutate(
      title_original = paste(.env$title_original, dplyr::row_number()),
      title_translation = paste(.env$title_translation, dplyr::row_number()),
      subject_tr = .env$subject_tr,
      subject_en = .env$subject_en
    )
}

tezr_example_irrigation_results <- function() {
  tezr_example_topic_results(
    title_original = "Tarimsal sulama verimliligi",
    title_translation = "Agricultural irrigation efficiency",
    subject_tr = "Ziraat; Ekonomi",
    subject_en = "Agriculture; Economics"
  )
}

tezr_example_household_results <- function() {
  tezr_example_topic_results(
    title_original = "Hanehalki gelir ve tuketim analizi",
    title_translation = "Household income and consumption analysis",
    subject_tr = "Ekonomi",
    subject_en = "Economics"
  )
}

tezr_example_econometrics_results <- function() {
  tezr_example_topic_results(
    title_original = "Ekonometri uygulamalari",
    title_translation = "Applications of econometrics",
    subject_tr = "Ekonometri; Ekonomi",
    subject_en = "Econometrics; Economics"
  )
}

tezr_example_europe_results <- function() {
  tezr_example_topic_results(
    title_original = "Avrupa Birligi ve Turkiye",
    title_translation = "European Union and Turkiye",
    subject_tr = "Avrupa Birligi; Ekonomi",
    subject_en = "European Union; Economics"
  )
}

tezr_example_detail_results <- function() {
  tibble::tibble(
    thesis_no = "9677551",
    title_original = "Parasal aktarim mekanizmasi cercevesinde ozel sektore kredi",
    title_translation = "Domestic credit to private sector and monetary transmission",
    author = "PERIHAN EZGI BALLI",
    advisor = "PROF. DR. HASAN SAHIN",
    co_advisor = NA_character_,
    university = "Ankara Universitesi",
    institute = "Sosyal Bilimler Enstitusu",
    division = "Iktisat Ana Bilim Dali",
    year = "2020",
    pages = "153",
    thesis_type_tr = "Doktora",
    thesis_type_en = "Doctorate",
    language_tr = "Turkce",
    language_en = "Turkish",
    subject_tr = "Ekonomi; Enerji",
    subject_en = "Economics; Energy",
    abstract_original = paste(
      "Enerji piyasasi duzenlemelerinin ana ekseninde enerji arz",
      "guvenligi, piyasa yapisi ve fiyatlama davranislari yer alir."
    ),
    abstract_translation = paste(
      "This thesis consists of three essays on energy market regulation,",
      "market design, and transformation."
    ),
    keywords_tr = "Enerji piyasalari; Duzenleme; Elektrik",
    keywords_en = "Energy markets; Regulation; Electricity",
    access_status = "open",
    pdf_url = paste0(
      "https://tez.yok.gov.tr/UlusalTezMerkezi/tezIndir.jsp?id=",
      "TCKf4ksTOVsOBqUcPYMKWQ"
    ),
    detail_url = paste0(
      "https://tez.yok.gov.tr/UlusalTezMerkezi/tezDetay.jsp?id=",
      "TCKf4ksTOVsOBqUcPYMKWQ"
    )
  )
}

tezr_example_ankara_phd_results <- function() {
  tezr_example_econ_theses() |>
    dplyr::mutate(
      university = "Ankara Universitesi",
      thesis_type_tr = "Doktora",
      thesis_type_en = "Doctorate",
      year = 2020L + dplyr::row_number() - 1L
    ) |>
    dplyr::slice_head(n = 6)
}

tezr_example_universities <- function() {
  tibble::tibble(
    name = c(
      "ANKARA UNIVERSITESI",
      "ISTANBUL UNIVERSITESI",
      "ORTA DOGU TEKNIK UNIVERSITESI"
    ),
    id = c("2", "3", "60")
  )
}

tezr_example_subjects <- function() {
  tibble::tibble(
    name_tr = c("Ekonomi", "Ekonometri", "Enerji"),
    name_en = c("Economics", "Econometrics", "Energy"),
    id = c("115", "116", "117")
  )
}

tezr_example_year_stats <- function() {
  tibble::tibble(
    year = 2021:2025,
    yuksek_lisans = c(28906L, 33102L, 35987L, 40124L, 22615L),
    doktora = c(7128L, 7481L, 8026L, 8841L, 4416L),
    toplam = c(36034L, 40583L, 44013L, 48965L, 27031L)
  )
}

tezr_example_type_stats <- function() {
  tibble::tibble(
    yuksek_lisans = 721384,
    doktora = 178264,
    tipta_uzmanlik = 90030,
    sanatta_yeterlik = 2395
  )
}

tezr_example_econ_theses <- function() {
  x <- tezr_example_search_results()
  dplyr::bind_rows(x, x) |>
    dplyr::mutate(
      thesis_no = paste0(.data$thesis_no, dplyr::row_number()),
      year = rep(2016:2023, length.out = dplyr::n()),
      university = rep(
        c(
          "Ankara Universitesi",
          "Istanbul Universitesi",
          "Marmara Universitesi",
          "Ege Universitesi"
        ),
        length.out = dplyr::n()
      ),
      thesis_type_en = rep(c("Master", "Doctorate"), length.out = dplyr::n()),
      subject_tr = "Ekonometri",
      subject_en = "Econometrics"
    )
}

tezr_example_climate_results <- function() {
  x <- tezr_example_search_results()
  dplyr::bind_rows(x, x, x) |>
    dplyr::mutate(
      thesis_no = paste0(.data$thesis_no, dplyr::row_number()),
      title_original = paste("Iklim degisikligi", dplyr::row_number()),
      title_translation = paste("Climate change", dplyr::row_number()),
      year = rep(2015:2026, length.out = dplyr::n()),
      university = rep(
        c(
          "Ankara Universitesi",
          "Istanbul Universitesi",
          "Ege Universitesi",
          "Marmara Universitesi"
        ),
        length.out = dplyr::n()
      ),
      thesis_type_en = rep(c("Master", "Doctorate"), length.out = dplyr::n()),
      subject_tr = "Cevre Bilimleri; Ekonomi",
      subject_en = "Environmental Sciences; Economics"
    )
}

tezr_example_ml_details <- function() {
  tibble::tibble(
    thesis_no = c("910001", "910002", "910003", "910004", "910005"),
    keywords_tr = c(
      "Makine ogrenmesi; Derin ogrenme; Siniflandirma",
      "Makine ogrenmesi; Yapay zeka; Tahmin",
      "Derin ogrenme; Goruntu isleme; Sinir aglari",
      "Makine ogrenmesi; Veri madenciligi; Siniflandirma",
      "Dogal dil isleme; Makine ogrenmesi; Metin madenciligi"
    )
  )
}
