# Tests for HTML parsing functions (parse.R)

# Mock HTML for search results page (JavaScript-based watable)
mock_search_results_html <- '
<!DOCTYPE html>
<html>
<head><title>Tez Arama Sonuclari</title></head>
<body>
<div id="content">
<p>Tarama sonucunda 42 kayıt bulundu.</p>
<script>
var rows = [];
var doc = {userId: "<span onclick=\\"tezDetay(\'abc123\',\'xyz789\')\\">942835</span>", name: "AHMET YILMAZ", age: "2023", weight: "Yapay Zeka ile <b>Veri</b> Analizi", important: "Doktora", someDate: "Bilgisayar Muhendisligi=Computer Engineering", uni: "Istanbul Teknik Universitesi", height: "Turkce"};
rows.push(doc);
var doc = {userId: "<span onclick=\\"tezDetay(\'def456\',\'uvw123\')\\">123456</span>", name: "MEHMET DEMIR", age: "2022", weight: "Makine Ogrenmesi Uygulamalari", important: "Yuksek Lisans", someDate: "Yazilim Muhendisligi=Software Engineering", uni: "Ankara Universitesi", height: "Ingilizce"};
rows.push(doc);
</script>
</div>
</body>
</html>
'

# Mock HTML for detail page (horizontal table structure)
mock_detail_html <- '
<!DOCTYPE html>
<html>
<head><title>Tez Detay</title></head>
<body>
<table>
<tr class="renkbas">
<td>Tez No</td>
<td>\u0130ndirme</td>
<td>Tez K\u00FCnye</td>
<td>Durumu</td>
</tr>
<tr class="renkp">
<td>942835</td>
<td><a href="pdf/12345.pdf">PDF \u0130ndir</a></td>
<td>Yapay Zeka ile Veri Analizi / Artificial Intelligence Data Analysis Yazar: AHMET YILMAZ Dani\u015Fman: Prof. Dr. MEHMET HOCA Yer Bilgisi: Istanbul Teknik Universitesi / Fen Bilimleri Enstitusu / Bilgisayar Muhendisligi Konu: Yapay Zeka Dizin: yapay zeka, veri analizi, makine ogrenmesi</td>
<td>2023 210 s. Turkce Onaylandi</td>
</tr>
</table>
<td id="td0">Bu tez, yapay zeka yontemlerini kullanarak veri analizi yapmaktadir. Anahtar Sozcukler: yapay zeka, veri, analiz</td>
<td id="td1">This thesis uses artificial intelligence methods for data analysis. Keywords: artificial intelligence, data, analysis</td>
</body>
</html>
'

# Mock HTML for detail page with co-advisor
mock_detail_html_coadvisor <- '
<!DOCTYPE html>
<html>
<head><title>Tez Detay</title></head>
<body>
<table>
<tr class="renkbas">
<td>Tez No</td>
<td>\u0130ndirme</td>
<td>Tez K\u00FCnye</td>
<td>Durumu</td>
</tr>
<tr class="renkp">
<td>944372</td>
<td><a href="pdf/99999.pdf">PDF \u0130ndir</a></td>
<td>Test Thesis / Test Thesis En Yazar: BAYRAM CERIT Dani\u015Fman: PROF. DR. MELEK ACAR ; DO\u00C7. DR. \u0130BRAH\u0130M \u00D6ZMEN Yer Bilgisi: Selcuk Universitesi / Sosyal Bilimler Enstitusu / Isletme ABD Konu: Ekonomi = Economics Dizin:</td>
<td>2025 145 s. Turkce Onaylandi</td>
</tr>
</table>
<td id="td0">Ozet metni. Anahtar Sozcukler: enerji, uretim</td>
<td id="td1">Abstract text. Keywords: energy, production</td>
</body>
</html>
'

# Mock HTML for an English-language thesis where the portal places
# the English original in td0 and the Turkish translation in td1.
mock_detail_html_english_swapped <- '
<!DOCTYPE html>
<html>
<head><title>Tez Detay</title></head>
<body>
<table>
<tr class="renkbas">
<td>Tez No</td>
<td>\u0130ndirme</td>
<td>Tez K\u00FCnye</td>
<td>Durumu</td>
</tr>
<tr class="renkp">
<td>123999</td>
<td><a href="pdf/99999.pdf">PDF \u0130ndir</a></td>
<td>Three essays in dynamic macroeconomics / Dinamik makroekonomi \u00FCzerine \u00FC\u00E7 makale Author: TEST AUTHOR Advisor: TEST ADVISOR Where: Test University / Institute / Economics</td>
<td>2024 180 s. Ingilizce Onaylandi</td>
</tr>
</table>
<td id="td0">This thesis consists of three essays in dynamic macroeconomics. Keywords: macroeconomics, essays</td>
<td id="td1">Bu tez dinamik makroekonomi \u00FCzerine \u00FC\u00E7 makaleden olu\u015Fur. Anahtar Sozcukler: makroekonomi, makaleler</td>
</body>
</html>
'

# Mock HTML for detail page (fallback: vertical labels only)
mock_detail_html_fallback <- "
<!DOCTYPE html>
<html>
<head><title>Tez Detay</title></head>
<body>
<table>
<tr><td>Konu</td><td>:</td><td>Ekonomi=Economics; Ekonometri=Econometrics</td></tr>
</table>
</body>
</html>
"

# Mock HTML with no results
mock_empty_results_html <- "
<!DOCTYPE html>
<html>
<body>
<p>Tarama sonucunda 0 kayit bulundu.</p>
<script>var rows = [];</script>
</body>
</html>
"

# Mock HTML for the redesigned 2026 result-card structure.
mock_result_cards_html <- '
<!DOCTYPE html>
<html>
<body>
<div class="result-limit-warning">
Tarama sonucunda 2.345 kayıt bulundu. 2.000 tanesi görüntülenmektedir.
</div>
<div class="result-card" data-kayitno="abc123" data-tezno="enc456" data-index="0">
  <div class="card-title">Yapay zeka ile veri analizi</div>
  <div class="card-info" style="font-style: italic;">Data analysis with artificial intelligence</div>
  <div class="card-info"><strong>Tez No:</strong> 1003627</div>
</div>
<div class="result-card" data-kayitno="def789" data-tezno="enc999" data-index="1">
  <div class="card-title">Makine ogrenmesi uygulamalari</div>
  <div class="card-info"><strong>Tez No:</strong> 1003628</div>
</div>
<script>
const referenceData = {
  "0": {"meta": {"author": "AYSE YILMAZ", "year": "2026", "subject": "Bilgisayar Muhendisligi=Computer Engineering", "type": "Doktora", "lang": "Turkce", "yer": "MARMARA UNIVERSITESI / "}},
  "1": {"meta": {"author": "MEHMET DEMIR", "year": "2025", "subject": "Yazilim Muhendisligi=Software Engineering", "type": "Yuksek Lisans", "lang": "Ingilizce", "yer": "ANKARA UNIVERSITESI / Fen Bilimleri Enstitusu / "}},
};
</script>
</body>
</html>
'

test_that("parse_results_table extracts records from JavaScript objects", {
  html <- rvest::read_html(mock_search_results_html)
  results <- parse_results_table(html)

  expect_s3_class(results, "tbl_df")
  expect_equal(nrow(results), 2)

  # Check first record
  expect_equal(results$thesis_no[1], "942835")
  expect_equal(results$author[1], "AHMET YILMAZ")
  expect_equal(results$year[1], 2023L)
  expect_equal(results$thesis_type_tr[1], "Doktora")
  expect_equal(results$subject_tr[1], "Bilgisayar Muhendisligi")
  expect_equal(results$subject_en[1], "Computer Engineering")
  expect_equal(results$detail_id[1], "abc123")

  # Check second record
  expect_equal(results$thesis_no[2], "123456")
  expect_equal(results$author[2], "MEHMET DEMIR")
  expect_equal(results$year[2], 2022L)
  expect_equal(results$subject_tr[2], "Yazilim Muhendisligi")
  expect_equal(results$subject_en[2], "Software Engineering")
})

test_that("parse_results_table extracts redesigned result cards", {
  html <- rvest::read_html(mock_result_cards_html)
  parsed_results <- parse_results_table(html)

  expect_s3_class(parsed_results, "tbl_df")
  expect_equal(nrow(parsed_results), 2)
  expect_equal(parsed_results$thesis_no, c("1003627", "1003628"))
  expect_equal(parsed_results$detail_id, c("abc123", "def789"))
  expect_equal(parsed_results$encrypted_no, c("enc456", "enc999"))
  expect_equal(parsed_results$author, c("AYSE YILMAZ", "MEHMET DEMIR"))
  expect_equal(parsed_results$year, c(2026L, 2025L))
  expect_equal(
    parsed_results$title_translation[1],
    "Data analysis with artificial intelligence"
  )
  expect_true(grepl("id=abc123", parsed_results$detail_url[1], fixed = TRUE))
  expect_true(grepl("no=enc456", parsed_results$detail_url[1], fixed = TRUE))
})

test_that("parse_results_table extracts English result-card thesis numbers", {
  html <- rvest::read_html(gsub(
    "Tez No",
    "Thesis No",
    mock_result_cards_html,
    fixed = TRUE
  ))
  parsed_results <- parse_results_table(html)

  expect_equal(parsed_results$thesis_no, c("1003627", "1003628"))
})

test_that("extract_reference_data handles large redesigned metadata blocks", {
  entries <- vapply(
    seq_len(2000),
    function(index) {
      sprintf(
        '"%d": {"meta": {"author": "AUTHOR %d", "year": "2026", "subject": "Ekonomi {hane}=Household } text", "type": "Doktora", "lang": "Turkce", "yer": "TEST UNIVERSITY / "}}',
        index - 1L,
        index
      )
    },
    character(1)
  )

  html_text <- paste0(
    "<script>const referenceData = {\n",
    paste(entries, collapse = ",\n"),
    "\n};</script><div>other page content</div>"
  )

  reference_data <- extract_reference_data(html_text)

  expect_length(reference_data, 2000)
  expect_equal(reference_data[["0"]]$meta$author, "AUTHOR 1")
  expect_equal(
    reference_data[["1999"]]$meta$subject,
    "Ekonomi {hane}=Household } text"
  )
})

test_that("parse_results_table handles closing brace characters inside field values", {
  html <- rvest::read_html(
    '
<!DOCTYPE html>
<html>
<body>
<p>Tarama sonucunda 2 kayit bulundu.</p>
<script>
var rows = [];
var doc = {userId: "<span onclick=\\"tezDetay(\'brace001\',\'token1\')\\">371511</span>", name: "ISTEM FER", age: "2011", weight: "Modeling climate change effects}{on forecasts", important: "Doktora", someDate: "Ekonomi=Economics", uni: "Test University", height: "Turkce"};
rows.push(doc);
var doc = {userId: "<span onclick=\\"tezDetay(\'brace002\',\'token2\')\\">371512</span>", name: "SECOND AUTHOR", age: "2012", weight: "Second title", important: "Yuksek Lisans", someDate: "Iklim=Climate", uni: "Another University", height: "Ingilizce"};
rows.push(doc);
</script>
</body>
</html>
'
  )

  results <- parse_results_table(html)

  expect_equal(nrow(results), 2)
  expect_equal(sort(results$thesis_no), c("371511", "371512"))
})

test_that("extract_js_doc_blocks captures full doc blocks and strips rows.push suffix", {
  html_text <- '
<script>
var rows = [];
var doc = {userId: "<span onclick=\\"tezDetay(\'brace001\',\'token1\')\\">371511</span>", name: "ISTEM FER", age: "2011", weight: "Modeling climate change effects}{on forecasts"};
rows.push(doc);
var doc = {userId: "<span onclick=\\"tezDetay(\'brace002\',\'token2\')\\">371512</span>", name: "SECOND AUTHOR", age: "2012", weight: "Second title"};
rows.push(doc);
</script>
'

  doc_blocks <- extract_js_doc_blocks(html_text)

  expect_equal(length(doc_blocks), 2)
  expect_true(all(grepl("^var doc = \\{", doc_blocks)))
  expect_false(any(grepl("rows\\.push\\(doc\\)", doc_blocks)))
})

test_that("parse_results_table returns empty tibble for no results", {
  html <- rvest::read_html(mock_empty_results_html)
  results <- parse_results_table(html)

  expect_s3_class(results, "tbl_df")
  expect_equal(nrow(results), 0)
  expect_true("thesis_no" %in% names(results))
  expect_true("detail_id" %in% names(results))
})

test_that("parse_detail_page extracts co-advisor when present", {
  html <- rvest::read_html(mock_detail_html_coadvisor)
  details <- parse_detail_page(html)

  expect_equal(details$advisor, "PROF. DR. MELEK ACAR")
  expect_equal(details$co_advisor, "DO\u00C7. DR. \u0130BRAH\u0130M \u00D6ZMEN")
})

test_that("parse_detail_page returns NA co_advisor when absent", {
  html <- rvest::read_html(mock_detail_html)
  details <- parse_detail_page(html)

  expect_equal(details$advisor, "Prof. Dr. MEHMET HOCA")
  expect_true(is.na(details$co_advisor))
})

test_that("parse_detail_page fallback preserves subjects", {
  html <- rvest::read_html(mock_detail_html_fallback)
  details <- parse_detail_page(html)

  expect_equal(details$subject_tr, "Ekonomi; Ekonometri")
  expect_equal(details$subject_en, "Economics; Econometrics")
})

test_that("parse_location_info extracts university, institute, division", {
  metadata <- "Yer Bilgisi: Istanbul Universitesi / Sosyal Bilimler Enstitusu / Iktisat"
  result <- parse_location_info(metadata)

  expect_equal(result$university, "Istanbul Universitesi")
  expect_equal(result$institute, "Sosyal Bilimler Enstitusu")
  expect_equal(result$division, "Iktisat")
})

test_that("empty_results_tibble has correct structure", {
  empty <- empty_results_tibble()

  expect_s3_class(empty, "tbl_df")
  expect_equal(nrow(empty), 0)
  expected_cols <- c(
    "thesis_no",
    "title_original",
    "title_translation",
    "author",
    "university",
    "year",
    "thesis_type_tr",
    "thesis_type_en",
    "language_tr",
    "language_en",
    "subject_tr",
    "subject_en",
    "detail_id"
  )
  expect_true(all(expected_cols %in% names(empty)))
})

test_that("parse_js_doc_fields extracts keyed values and get_doc_field returns cleaned values", {
  doc_str <- paste0(
    "var doc = {",
    "name: \"test\\\\'s value\", ",
    "age: \"2024\", ",
    "weight: \"line1\\\\nline2\"",
    "};"
  )

  parsed_fields <- parse_js_doc_fields(doc_str)

  expect_equal(get_doc_field(parsed_fields, "name"), "test's value")
  expect_equal(get_doc_field(parsed_fields, "age"), "2024")
  expect_equal(get_doc_field(parsed_fields, "weight"), "line1 line2")
  expect_true(is.na(get_doc_field(parsed_fields, "missing_field")))
})

test_that("normalize_js_field_values only cleans values that need decoding", {
  raw_values <- c(
    "plain",
    " has spaces ",
    "line1\\\\nline2",
    "quote\\\\\\\"test"
  )

  normalized_values <- normalize_js_field_values(raw_values)

  expect_equal(normalized_values[1], "plain")
  expect_equal(normalized_values[2], "has spaces")
  expect_equal(normalized_values[3], "line1 line2")
  expect_equal(normalized_values[4], "quote\"test")
})

test_that("strip_html_tags_if_present strips only values containing tags", {
  title_values <- c("Plain Text", "<b>Tagged</b> Title")

  stripped_values <- strip_html_tags_if_present(title_values)

  expect_equal(stripped_values[1], "Plain Text")
  expect_equal(stripped_values[2], "Tagged Title")
})

test_that("parse_title_fields_fast strips tags and splits bilingual titles", {
  title_raw <- "Turkce <b>Baslik</b><br/>English <i>Title</i>"
  parsed_title <- parse_title_fields_fast(title_raw)

  expect_equal(parsed_title$primary, "Turkce Baslik")
  expect_equal(parsed_title$secondary, "English Title")
})

test_that("parse_title_fields_fast returns NA secondary when title has one part", {
  title_raw <- "Tek Baslik"
  parsed_title <- parse_title_fields_fast(title_raw)

  expect_equal(parsed_title$primary, "Tek Baslik")
  expect_true(is.na(parsed_title$secondary))
})


test_that("extract_total_count extracts count from page text", {
  html <- rvest::read_html(mock_search_results_html)
  count <- extract_total_count(html)
  expect_equal(count, 42L)
})

test_that("extract_total_count handles redesigned dotted thousands text", {
  html <- rvest::read_html(mock_result_cards_html)
  count <- extract_total_count(html)
  expect_equal(count, 2345L)
})

test_that("extract_total_count handles English records-found text", {
  html <- rvest::read_html("<html><body>1.404 records found.</body></html>")
  count <- extract_total_count(html)
  expect_equal(count, 1404L)
})

test_that("extract_total_count returns 0 for missing count", {
  html <- rvest::read_html("<html><body>No results info</body></html>")
  count <- extract_total_count(html)
  expect_equal(count, 0L)
})

test_that("split_titles separates primary and secondary titles", {
  result <- split_titles("Turkce Baslik / English Title")
  expect_equal(result$primary, "Turkce Baslik")
  expect_equal(result$secondary, "English Title")
})

test_that("split_titles handles single title", {
  result <- split_titles("Only Turkish Title")
  expect_equal(result$primary, "Only Turkish Title")
  expect_true(is.na(result$secondary))
})

test_that("split_titles handles empty/NA input", {
  result <- split_titles(NA_character_)
  expect_true(is.na(result$primary))
  expect_true(is.na(result$secondary))

  result <- split_titles("")
  expect_true(is.na(result$primary))
})

test_that("split_titles handles multilingual theses correctly", {
  # Arabic thesis: primary=Arabic, secondary=Turkish translation
  arabic_result <- split_titles(
    "التفكر والتأمل عند الصوفية / Tasavvuf ve zen Budizmi"
  )
  expect_equal(arabic_result$primary, "التفكر والتأمل عند الصوفية")
  expect_equal(arabic_result$secondary, "Tasavvuf ve zen Budizmi")

  # Chinese thesis: primary=Chinese, secondary=Turkish translation
  chinese_result <- split_titles(
    "老子與古蘭經的比較研究 / Laozi ve Kuran karşılaştırması"
  )
  expect_equal(chinese_result$primary, "老子與古蘭經的比較研究")
  expect_equal(chinese_result$secondary, "Laozi ve Kuran karşılaştırması")

  # Russian thesis: primary=Russian (Cyrillic), secondary=Turkish translation
  russian_result <- split_titles(
    "Лингво-стилистические особенности / Tuzak kelimelerin özellikleri"
  )
  expect_equal(russian_result$primary, "Лингво-стилистические особенности")
  expect_equal(russian_result$secondary, "Tuzak kelimelerin özellikleri")

  # English thesis: primary=English, secondary=Turkish translation
  english_result <- split_titles(
    "The Malaysian Islamic authorities / Malezya dini kurumları"
  )
  expect_equal(english_result$primary, "The Malaysian Islamic authorities")
  expect_equal(english_result$secondary, "Malezya dini kurumları")

  # Japanese thesis: primary=Japanese (Hiragana/Kanji), secondary=Turkish translation
  japanese_result <- split_titles(
    "依頼と誘いに対する断り / Reddetme stratejileri"
  )
  expect_equal(japanese_result$primary, "依頼と誘いに対する断り")
  expect_equal(japanese_result$secondary, "Reddetme stratejileri")

  # Greek thesis: primary=Greek (Greek alphabet), secondary=Turkish translation
  greek_result <- split_titles(
    "H kως κατα την βυζαντινη / Bizans döneminde Kos adası"
  )
  expect_equal(greek_result$primary, "H kως κατα την βυζαντινη")
  expect_equal(greek_result$secondary, "Bizans döneminde Kos adası")

  # Korean thesis: primary=Korean (Hangul script), secondary=Turkish translation
  korean_hangul_result <- split_titles(
    "터키 인 학습자를 위한 한국어 양태부사 / Türk öğrenciler için korece"
  )
  expect_equal(
    korean_hangul_result$primary,
    "터키 인 학습자를 위한 한국어 양태부사"
  )
  expect_equal(korean_hangul_result$secondary, "Türk öğrenciler için korece")

  # Turkish thesis: primary=Turkish, secondary=English translation
  turkish_result <- split_titles("Türkiye'de hanehalkı / Household in Turkiye")
  expect_equal(turkish_result$primary, "Türkiye'de hanehalkı")
  expect_equal(turkish_result$secondary, "Household in Turkiye")
})

test_that("split_titles handles thesis without translation", {
  # Some theses only have title in original language, no translation
  # Real example: Japanese thesis 708017 by BERNA ARIKAN
  japanese_single <- split_titles(
    "依頼と誘いに対する断りにおけるトルコ人日本語学習者の特徴"
  )
  expect_equal(
    japanese_single$primary,
    "依頼と誘いに対する断りにおけるトルコ人日本語学習者の特徴"
  )
  expect_true(is.na(japanese_single$secondary))

  # Greek thesis without translation: 611709 by NIKOLAOS KONTOGIANNIS
  greek_single <- split_titles(
    "H kως κατα την βυζαντινη περιοδο και την ιπποτοκρατια"
  )
  expect_equal(
    greek_single$primary,
    "H kως κατα την βυζαντινη περιοδο και την ιπποτοκρατια"
  )
  expect_true(is.na(greek_single$secondary))

  # Korean thesis with English title only: 707193 by MERT SABRİ KARAMAN
  # Note: Some theses written in one language may only have metadata in another
  korean_english <- split_titles(
    "The US strategy for Korea and ROK-US mutual defense treaty"
  )
  expect_equal(
    korean_english$primary,
    "The US strategy for Korea and ROK-US mutual defense treaty"
  )
  expect_true(is.na(korean_english$secondary))

  # Korean thesis with Turkish title only: 622416 by HANGYOUN CHO
  # Another variation: Korean thesis with only Turkish metadata
  korean_turkish <- split_titles(
    "Kompleks tedavi açısından incelenen Kore Şaman efsaneleri"
  )
  expect_equal(
    korean_turkish$primary,
    "Kompleks tedavi açısından incelenen Kore Şaman efsaneleri"
  )
  expect_true(is.na(korean_turkish$secondary))

  # English thesis with Turkish title only: 626102 by DURSUN EŞSİZ
  # Another variation: English thesis with only Turkish metadata
  english_turkish <- split_titles(
    "Korece öğrenen Türk öğrenciler için Korece duygu kelimeleri"
  )
  expect_equal(
    english_turkish$primary,
    "Korece öğrenen Türk öğrenciler için Korece duygu kelimeleri"
  )
  expect_true(is.na(english_turkish$secondary))
})

test_that("split_titles keeps slash-separated single-language titles intact", {
  result <- split_titles(
    "Aggregation and welfare analysis with mixed continuous/discrete choice models"
  )

  expect_equal(
    result$primary,
    "Aggregation and welfare analysis with mixed continuous/discrete choice models"
  )
  expect_true(is.na(result$secondary))
})

test_that("split_titles removes dangling trailing slash from single title", {
  result <- split_titles(
    "Aggregation and welfare analysis with mixed continuous/discrete choice models /"
  )

  expect_equal(
    result$primary,
    "Aggregation and welfare analysis with mixed continuous/discrete choice models"
  )
  expect_true(is.na(result$secondary))
})

test_that("parse_detail_page adds original and translation aliases for titles and abstracts", {
  html <- rvest::read_html(mock_detail_html)
  details <- parse_detail_page(html)

  expect_equal(details$title_original, "Yapay Zeka ile Veri Analizi")
  expect_equal(
    details$title_translation,
    "Artificial Intelligence Data Analysis"
  )
  expect_true(grepl("^Bu tez", details$abstract_original))
  expect_true(grepl("^This thesis", details$abstract_translation))
})

test_that("parse_detail_page maps English thesis abstracts correctly when td blocks are swapped", {
  html <- rvest::read_html(mock_detail_html_english_swapped)
  details <- parse_detail_page(html)

  expect_true(grepl("^This thesis", details$abstract_original))
  expect_true(grepl("^Bu tez", details$abstract_translation))
})

test_that("split_bilingual_subjects separates Turkish and English subjects", {
  result <- split_bilingual_subjects(
    "Bilgisayar Muhendisligi=Computer Engineering"
  )
  expect_equal(result$subject_tr, "Bilgisayar Muhendisligi")
  expect_equal(result$subject_en, "Computer Engineering")
})

test_that("split_bilingual_subjects handles multiple subjects", {
  result <- split_bilingual_subjects(
    "Ekonomi=Economics; Ekonometri=Econometrics"
  )
  expect_equal(result$subject_tr, "Ekonomi; Ekonometri")
  expect_equal(result$subject_en, "Economics; Econometrics")
})

test_that("split_bilingual_subjects handles subject without English translation", {
  result <- split_bilingual_subjects("Ekonomi")
  expect_equal(result$subject_tr, "Ekonomi")
  expect_true(is.na(result$subject_en))
})

test_that("split_bilingual_subjects handles empty/NA input", {
  result <- split_bilingual_subjects(NA_character_)
  expect_true(is.na(result$subject_tr))
  expect_true(is.na(result$subject_en))

  result <- split_bilingual_subjects("")
  expect_true(is.na(result$subject_tr))
})

test_that("parse_bilingual_entries splits bilingual pairs", {
  result <- parse_bilingual_entries(
    "Ekonomi=Economics; Ekonometri = Econometrics"
  )
  expect_equal(result$tr, c("Ekonomi", "Ekonometri"))
  expect_equal(result$en, c("Economics", "Econometrics"))
})

test_that("split_advisors separates advisor and co-advisor", {
  result <- split_advisors(
    "PROF. DR. MELEK ACAR ; DO\u00C7. DR. \u0130BRAH\u0130M \u00D6ZMEN"
  )
  expect_equal(result$advisor, "PROF. DR. MELEK ACAR")
  expect_equal(result$co_advisor, "DO\u00C7. DR. \u0130BRAH\u0130M \u00D6ZMEN")
})

test_that("split_advisors handles single advisor", {
  result <- split_advisors("PROF. DR. HASAN \u015EAH\u0130N")
  expect_equal(result$advisor, "PROF. DR. HASAN \u015EAH\u0130N")
  expect_true(is.na(result$co_advisor))
})

test_that("split_advisors handles NA and empty input", {
  result <- split_advisors(NA_character_)
  expect_true(is.na(result$advisor))
  expect_true(is.na(result$co_advisor))

  result <- split_advisors("")
  expect_true(is.na(result$advisor))
  expect_true(is.na(result$co_advisor))
})

test_that("split_advisors handles multiple co-advisors", {
  result <- split_advisors("A ; B ; C")
  expect_equal(result$advisor, "A")
  expect_equal(result$co_advisor, "B; C")
})

test_that("extract_td_text returns both td blocks", {
  html <- rvest::read_html(mock_detail_html)
  blocks <- extract_td_text(html)

  expect_equal(
    blocks$tr,
    paste0(
      "Bu tez, yapay zeka yontemlerini kullanarak veri analizi yapmaktadir. ",
      "Anahtar Sozcukler: yapay zeka, veri, analiz"
    )
  )
  expect_equal(
    blocks$en,
    paste0(
      "This thesis uses artificial intelligence methods for data analysis. ",
      "Keywords: artificial intelligence, data, analysis"
    )
  )
})

test_that("extract_stat_year extracts year from stats text", {
  expect_equal(extract_stat_year("2023 210 s. Turkce"), "2023")
  expect_equal(extract_stat_year("Onaylandi 1999"), "1999")
  expect_true(is.na(extract_stat_year("no year here")))
})

test_that("extract_stat_pages extracts page count", {
  expect_equal(extract_stat_pages("2023 210 s. Turkce"), "210")
  expect_equal(extract_stat_pages("150 s."), "150")
  expect_true(is.na(extract_stat_pages("no pages")))
})

test_that("extract_stat_language extracts language", {
  # Function returns list with tr and en fields
  result <- extract_stat_language("2023 210 s. T\u00FCrk\u00E7e")
  expect_equal(result$tr, "T\u00FCrk\u00E7e")
  expect_equal(result$en, "Turkish")

  result <- extract_stat_language("\u0130ngilizce")
  expect_equal(result$tr, "\u0130ngilizce")
  expect_equal(result$en, "English")

  result <- extract_stat_language("Unknown lang")
  expect_true(is.na(result$tr))
  expect_true(is.na(result$en))
})

test_that("extract_bilingual_lookup accepts cache_key and keeps behavior", {
  reference_data <- tibble::tibble(
    label_tr = c("T\u00FCrk\u00E7e", "\u0130ngilizce"),
    label_en = c("Turkish", "English")
  )

  first_match <- extract_bilingual_lookup(
    "2023 210 s. T\u00FCrk\u00E7e",
    reference_data,
    cache_key = "parse_test_lang_cache"
  )
  second_match <- extract_bilingual_lookup(
    "\u0130ngilizce",
    reference_data,
    cache_key = "parse_test_lang_cache"
  )

  expect_equal(first_match$tr, "T\u00FCrk\u00E7e")
  expect_equal(first_match$en, "Turkish")
  expect_equal(second_match$tr, "\u0130ngilizce")
  expect_equal(second_match$en, "English")
})

test_that("extract_bilingual_lookup caches repeated text results by cache key", {
  reference_data <- tibble::tibble(
    label_tr = c("T\u00FCrk\u00E7e", "\u0130ngilizce"),
    label_en = c("Turkish", "English")
  )

  cache_key <- "parse_test_lang_result_cache"
  text_value <- "\u0130ngilizce"
  result_key <- build_bilingual_lookup_result_key(cache_key, text_value)

  if (
    exists(result_key, envir = bilingual_lookup_result_cache, inherits = FALSE)
  ) {
    rm(list = result_key, envir = bilingual_lookup_result_cache)
  }

  first_match <- extract_bilingual_lookup(
    text_value,
    reference_data,
    cache_key = cache_key
  )
  second_match <- extract_bilingual_lookup(
    text_value,
    reference_data,
    cache_key = cache_key
  )

  expect_true(exists(
    result_key,
    envir = bilingual_lookup_result_cache,
    inherits = FALSE
  ))
  expect_equal(first_match$tr, second_match$tr)
  expect_equal(first_match$en, second_match$en)
})

test_that("coalesce operator handles NULL, NA, and empty strings", {
  `%|na|%` <- get("%|na|%", envir = asNamespace("tezr"))
  expect_equal(NULL %|na|% "default", "default")
  expect_equal(NA %|na|% "default", "default")
  expect_equal("" %|na|% "default", "default")
  expect_equal("value" %|na|% "default", "value")
  expect_equal(0 %|na|% "default", 0)
})

test_that("extract_keywords_from_index with mixed bilingual/monolingual entries", {
  # Mixed: some entries have English translations, some do not
  mixed_index <- "yapay zeka=artificial intelligence; veri analizi"
  result <- extract_keywords_from_index(mixed_index)

  # Only bilingual entries (with =) are included
  expect_equal(result$tr, "yapay zeka")
  expect_equal(result$en, "artificial intelligence")
})

test_that("extract_keywords_from_index returns NA for empty input", {
  result <- extract_keywords_from_index(NA_character_)
  expect_true(is.na(result$tr))
  expect_true(is.na(result$en))

  result2 <- extract_keywords_from_index("")
  expect_true(is.na(result2$tr))
  expect_true(is.na(result2$en))
})

test_that("merge_keywords deduplicates case-insensitively", {
  result <- merge_keywords("Yapay Zeka; veri", "yapay zeka; analiz")

  keywords <- strsplit(result, "; ")[[1]]
  # Should have 3 unique keywords (Yapay Zeka, veri, analiz)
  expect_equal(length(keywords), 3)
  # Original case preserved for first occurrence
  expect_true("Yapay Zeka" %in% keywords)
})

test_that("merge_keywords returns NA for empty inputs", {
  result <- merge_keywords(NA_character_, NA_character_)
  expect_true(is.na(result))

  result2 <- merge_keywords("", "")
  expect_true(is.na(result2))
})

test_that("extract_access_status returns open when pdf_link present", {
  html <- rvest::read_html("<html><body></body></html>")
  result <- extract_access_status(
    html,
    pdf_link = "http://example.com/thesis.pdf"
  )
  expect_equal(result, "open")
})

test_that("extract_access_status returns restricted for restricted text", {
  html <- rvest::read_html("<html><body>\u0130zinsiz</body></html>")
  result <- extract_access_status(html, pdf_link = NULL)
  expect_equal(result, "restricted")
})

test_that("extract_access_status returns NA for unknown", {
  html <- rvest::read_html("<html><body>Nothing relevant</body></html>")
  result <- extract_access_status(html, pdf_link = NULL)
  expect_true(is.na(result))
})
