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
var doc = {
userId: "<span onclick=\\"tezDetay(\'abc123\',\'xyz789\')\\">942835</span>",
name: "AHMET YILMAZ",
age: "2023",
weight: "Yapay Zeka ile <b>Veri</b> Analizi",
important: "Doktora",
someDate: "Bilgisayar Muhendisligi=Computer Engineering",
uni: "Istanbul Teknik Universitesi",
height: "Turkce"
};
rows.push(doc);
var doc = {
userId: "<span onclick=\\"tezDetay(\'def456\',\'uvw123\')\\">123456</span>",
name: "MEHMET DEMIR",
age: "2022",
weight: "Makine Ogrenmesi Uygulamalari",
important: "Yuksek Lisans",
someDate: "Yazilim Muhendisligi=Software Engineering",
uni: "Ankara Universitesi",
height: "Ingilizce"
};
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
<td>
Yapay Zeka ile Veri Analizi / Artificial Intelligence Data Analysis
Yazar: AHMET YILMAZ Dani\u015Fman: Prof. Dr. MEHMET HOCA
Yer Bilgisi: Istanbul Teknik Universitesi / Fen Bilimleri Enstitusu /
Bilgisayar Muhendisligi Konu: Yapay Zeka Dizin: yapay zeka,
veri analizi, makine ogrenmesi
</td>
<td>2023 210 s. Turkce Onaylandi</td>
</tr>
</table>
<td id="td0">
Bu tez, yapay zeka yontemlerini kullanarak veri analizi yapmaktadir.
Anahtar Sozcukler: yapay zeka, veri, analiz
</td>
<td id="td1">
This thesis uses artificial intelligence methods for data analysis.
Keywords: artificial intelligence, data, analysis
</td>
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
<td>
Test Thesis / Test Thesis En Yazar: BAYRAM CERIT
Dani\u015Fman: PROF. DR. MELEK ACAR ; DO\u00C7. DR. \u0130BRAH\u0130M \u00D6ZMEN
Yer Bilgisi: Selcuk Universitesi / Sosyal Bilimler Enstitusu /
Isletme ABD Konu: Ekonomi = Economics Dizin:
</td>
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
<td>
Three essays in dynamic macroeconomics / Dinamik makroekonomi \u00FCzerine
\u00FC\u00E7 makale Author: TEST AUTHOR Advisor: TEST ADVISOR
Where: Test University / Institute / Economics
</td>
<td>2024 180 s. Ingilizce Onaylandi</td>
</tr>
</table>
<td id="td0">
This thesis consists of three essays in dynamic macroeconomics.
Keywords: macroeconomics, essays
</td>
<td id="td1">
Bu tez dinamik makroekonomi \u00FCzerine \u00FC\u00E7 makaleden olu\u015Fur.
Anahtar Sozcukler: makroekonomi, makaleler
</td>
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
<tr>
<td>Konu</td>
<td>:</td>
<td>Ekonomi=Economics; Ekonometri=Econometrics</td>
</tr>
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

test_that("parse_results_table extracts records from JavaScript objects", {
  html <- rvest::read_html(mock_search_results_html)
  results <- parse_results_table(html)

  expect_s3_class(results, "tbl_df")
  expect_identical(nrow(results), 2L)

  # Check first record
  expect_identical(results$thesis_no[1], "942835")
  expect_identical(results$author[1], "AHMET YILMAZ")
  expect_identical(results$year[1], 2023L)
  expect_identical(results$thesis_type_tr[1], "Doktora")
  expect_identical(results$subject_tr[1], "Bilgisayar Muhendisligi")
  expect_identical(results$subject_en[1], "Computer Engineering")
  expect_identical(results$detail_id[1], compose_detail_id("abc123", "xyz789"))

  # Check second record
  expect_identical(results$thesis_no[2], "123456")
  expect_identical(results$author[2], "MEHMET DEMIR")
  expect_identical(results$year[2], 2022L)
  expect_identical(results$subject_tr[2], "Yazilim Muhendisligi")
  expect_identical(results$subject_en[2], "Software Engineering")
  expect_identical(results$detail_id[2], compose_detail_id("def456", "uvw123"))
})

test_that("parse_results_table extracts records from modern result cards", {
  html <- rvest::read_html(
    paste0(
      "<html><body>",
      "<div id='results-body'>",
      "<div class='result-card' data-index='0' ",
      "data-kayitno='kayit-abc' data-tezno='tez-xyz'>",
      "<div class='card-title'>Türkçe başlık</div>",
      "<div class='card-info' style='font-style: italic'>English title</div>",
      "<div class='card-info'><strong>Tez No:</strong> 1006368</div>",
      "</div>",
      "<div class='result-card' data-index='1' ",
      "data-kayitno='kayit-def' data-tezno='tez-uvw'>",
      "<div class='card-title'>Second title</div>",
      "<div class='card-info'><strong>Thesis No:</strong> 1001374</div>",
      "</div>",
      "</div>",
      "<script>",
      "const referenceData = {",
      "\"0\": {\"meta\": {",
      "\"author\": \"HASAN BURAK ÇALIYURT\",",
      "\"year\": \"2026\",",
      "\"subject\": \"Ekonometri;Ekonomi\",",
      "\"type\": \"Doktora\",",
      "\"lang\": \"Türkçe\",",
      "\"yer\": \"İSTANBUL ÜNİVERSİTESİ / ",
      "SOSYAL BİLİMLER ENSTİTÜSÜ / İKTİSAT\",",
      "\"title\": \"Türkçe başlık\"",
      "}},",
      "\"1\": {\"meta\": {",
      "\"author\": \"UĞUR SEVER\",",
      "\"year\": \"2026\",",
      "\"subject\": \"Ekonometri\",",
      "\"type\": \"Yüksek Lisans\",",
      "\"lang\": \"Türkçe\",",
      "\"yer\": \"BAYBURT ÜNİVERSİTESİ / LİSANSÜSTÜ EĞİTİM ENSTİTÜSÜ\",",
      "\"title\": \"Second title\"",
      "}}",
      "};",
      "</script>",
      "</body></html>"
    )
  )

  results <- parse_results_table(html)

  expect_s3_class(results, "tbl_df")
  expect_identical(nrow(results), 2L)
  expect_identical(results$thesis_no, c("1006368", "1001374"))
  expect_identical(results$title_original[1], "Türkçe başlık")
  expect_identical(results$title_translation[1], "English title")
  expect_identical(results$author[1], "HASAN BURAK ÇALIYURT")
  expect_identical(results$university[1], "İSTANBUL ÜNİVERSİTESİ")
  expect_identical(results$year[1], 2026L)
  expect_identical(results$thesis_type_tr[1], "Doktora")
  expect_identical(results$thesis_type_en[1], "Doctorate")
  expect_identical(results$language_tr[1], "Türkçe")
  expect_identical(results$language_en[1], "Turkish")
  expect_identical(results$subject_tr[1], "Ekonometri; Ekonomi")
  expect_true(is.na(results$subject_en[1]))
  expect_identical(
    results$detail_id[1],
    compose_detail_id("kayit-abc", "tez-xyz")
  )
  expect_identical(results$title_original[2], "Second title")
  expect_identical(results$title_translation[2], NA_character_)
  expect_identical(results$author[2], "UĞUR SEVER")
  expect_identical(results$university[2], "BAYBURT ÜNİVERSİTESİ")
  expect_identical(results$year[2], 2026L)
  expect_identical(results$thesis_type_tr[2], "Yüksek Lisans")
  expect_identical(results$thesis_type_en[2], "Master")
  expect_identical(results$language_tr[2], "Türkçe")
  expect_identical(results$language_en[2], "Turkish")
  expect_identical(results$subject_tr[2], "Ekonometri")
  expect_true(is.na(results$subject_en[2]))
  expect_identical(
    results$detail_id[2],
    compose_detail_id("kayit-def", "tez-uvw")
  )
})

test_that("parse_results_table handles braces in field values", {
  html <- rvest::read_html(
    '
<!DOCTYPE html>
<html>
<body>
<p>Tarama sonucunda 2 kayit bulundu.</p>
<script>
var rows = [];
var doc = {
userId: "<span onclick=\\"tezDetay(\'brace001\',\'token1\')\\">371511</span>",
name: "ISTEM FER",
age: "2011",
weight: "Modeling climate change effects}{on forecasts",
important: "Doktora",
someDate: "Ekonomi=Economics",
uni: "Test University",
height: "Turkce"
};
rows.push(doc);
var doc = {
userId: "<span onclick=\\"tezDetay(\'brace002\',\'token2\')\\">371512</span>",
name: "SECOND AUTHOR",
age: "2012",
weight: "Second title",
important: "Yuksek Lisans",
someDate: "Iklim=Climate",
uni: "Another University",
height: "Ingilizce"
};
rows.push(doc);
</script>
</body>
</html>
'
  )

  results <- parse_results_table(html)

  expect_identical(nrow(results), 2L)
  expect_identical(sort(results$thesis_no), c("371511", "371512"))
})

test_that("extract_js_doc_blocks captures blocks and strips rows.push suffix", {
  html_text <- '
<script>
var rows = [];
var doc = {
userId: "<span onclick=\\"tezDetay(\'brace001\',\'token1\')\\">371511</span>",
name: "ISTEM FER",
age: "2011",
weight: "Modeling climate change effects}{on forecasts"
};
rows.push(doc);
var doc = {
userId: "<span onclick=\\"tezDetay(\'brace002\',\'token2\')\\">371512</span>",
name: "SECOND AUTHOR",
age: "2012",
weight: "Second title"
};
rows.push(doc);
</script>
'

  doc_blocks <- extract_js_doc_blocks(html_text)

  expect_length(doc_blocks, 2L)
  expect_true(all(grepl("^var doc = \\{", doc_blocks)))
  expect_false(any(grepl("rows.push(doc)", doc_blocks, fixed = TRUE)))
})

test_that("detail id helpers preserve legacy and paired identifiers", {
  expect_identical(compose_detail_id("old-id", NA_character_), "old-id")
  expect_identical(compose_detail_id("old-id", ""), "old-id")

  paired <- compose_detail_id("kayit-abc", "tez-xyz")
  expect_identical(paired, "kayit-abc|tez-xyz")

  decoded <- split_detail_id(paired)
  expect_identical(decoded$id, "kayit-abc")
  expect_identical(decoded$no, "tez-xyz")

  legacy <- split_detail_id("old-id")
  expect_identical(legacy$id, "old-id")
  expect_identical(legacy$no, NA_character_)
})

test_that("modern result helpers handle empty fallback paths", {
  expect_identical(
    compose_detail_id(NA_character_, "encrypted-no"),
    NA_character_
  )
  expect_identical(
    split_detail_id(NA_character_),
    list(id = NA_character_, no = NA_character_)
  )
  expect_identical(
    parse_js_quoted_fields("not a JavaScript object"),
    stats::setNames(character(), character())
  )
  expect_identical(extract_modern_reference_data("<html></html>"), list())
  expect_identical(
    extract_modern_reference_data(
      'const referenceData = {"key": {"meta": {"author": "A"}}};'
    ),
    list()
  )

  html <- rvest::read_html(
    "<html><body><div class='result-card'></div></body></html>"
  )

  expect_identical(nrow(parse_modern_result_cards(html)), 0L)
  expect_identical(parse_modern_university(NA_character_), NA_character_)
  expect_identical(normalize_modern_subject(NA_character_), NA_character_)
})

test_that("parse_results_table returns empty tibble for no results", {
  html <- rvest::read_html(mock_empty_results_html)
  results <- parse_results_table(html)

  expect_s3_class(results, "tbl_df")
  expect_identical(nrow(results), 0L)
  expect_true("thesis_no" %in% names(results))
  expect_true("detail_id" %in% names(results))
})

test_that("parse_results_table returns empty when doc blocks are unparseable", {
  html <- rvest::read_html(
    paste0(
      "<html><script>",
      "var doc = { name: \"No user\" }; rows.push(doc);",
      "</script></html>"
    )
  )
  results <- parse_results_table(html)

  expect_s3_class(results, "tbl_df")
  expect_identical(nrow(results), 0L)
})

test_that("parse_detail_page extracts co-advisor when present", {
  html <- rvest::read_html(mock_detail_html_coadvisor)
  details <- parse_detail_page(html)

  expect_identical(details$advisor, "PROF. DR. MELEK ACAR")
  expect_identical(
    details$co_advisor,
    "DO\u00C7. DR. \u0130BRAH\u0130M \u00D6ZMEN"
  )
})

test_that("parse_detail_page returns NA co_advisor when absent", {
  html <- rvest::read_html(mock_detail_html)
  details <- parse_detail_page(html)

  expect_identical(details$advisor, "Prof. Dr. MEHMET HOCA")
  expect_true(is.na(details$co_advisor))
})

test_that("parse_detail_page fallback preserves subjects", {
  html <- rvest::read_html(mock_detail_html_fallback)
  details <- parse_detail_page(html)

  expect_identical(details$subject_tr, "Ekonomi; Ekonometri")
  expect_identical(details$subject_en, "Economics; Econometrics")
})

test_that("detail PDF links handle optional link and href nodes", {
  no_link <- rvest::read_html("<td>PDF yok</td>") |>
    rvest::html_element("td")
  no_href <- rvest::read_html("<td><a>PDF</a></td>") |>
    rvest::html_element("td")

  expect_true(is.na(detail_pdf_link(NULL)))
  expect_true(is.na(detail_pdf_link(no_link)))
  expect_true(is.na(detail_pdf_link(no_href)))
})

test_that("extract_detail_field handles table and label layouts", {
  html <- rvest::read_html(
    paste0(
      "<html><body>",
      "<table>",
      "<tr><td>Author</td><td>Direct Value</td></tr>",
      "<tr><td>Subject</td><td>:</td><td>Second Value</td></tr>",
      "</table>",
      "<div><span class='label'>Language</span>",
      "<span>Label Sibling</span></div>",
      "</body></html>"
    )
  )

  expect_identical(extract_detail_field(html, "Author"), "Direct Value")
  expect_identical(extract_detail_field(html, "Subject"), "Second Value")
  expect_identical(extract_detail_field(html, "Language"), "Label Sibling")
  expect_true(is.na(extract_detail_field(html, "Missing")))

  missing_second_sibling <- rvest::read_html(
    paste0(
      "<html><body><table>",
      "<tr><td>Subject</td><td>:</td></tr>",
      "</table></body></html>"
    )
  )
  expect_true(is.na(extract_detail_field(missing_second_sibling, "Subject")))
})

test_that("original-language matching covers known script families", {
  expect_true(matches_original_language("Türkçe özet", "Turkish"))
  expect_true(matches_original_language("English abstract", "English"))
  expect_true(matches_original_language("التفكر والتأمل", "Arabic"))
  expect_true(matches_original_language("Кириллический текст", "Kirghiz"))
  expect_true(matches_original_language("Кириллический текст", "Russian"))
  expect_true(matches_original_language("Кириллический текст", "Kazakh"))
  expect_false(matches_original_language("", "English"))
  expect_false(matches_original_language("plain text", "Unknown"))
  expect_identical(
    original_language_candidate("Türkçe özet", "Turkish"),
    "Türkçe özet"
  )
})

test_that("parse_location_info extracts university, institute, division", {
  metadata <- paste0(
    "Yer Bilgisi: Istanbul Universitesi / Sosyal Bilimler Enstitusu / ",
    "Iktisat"
  )
  result <- parse_location_info(metadata)

  expect_identical(result$university, "Istanbul Universitesi")
  expect_identical(result$institute, "Sosyal Bilimler Enstitusu")
  expect_identical(result$division, "Iktisat")
})

test_that("empty_results_tibble has correct structure", {
  empty <- empty_results_tibble()

  expect_s3_class(empty, "tbl_df")
  expect_identical(nrow(empty), 0L)
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

test_that("parse_js_doc_fields extracts keyed values and cleans field values", {
  doc_str <- paste0(
    "var doc = {",
    "name: \"test\\\\'s value\", ",
    "age: \"2024\", ",
    "weight: \"line1\\\\nline2\"",
    "};"
  )

  parsed_fields <- parse_js_doc_fields(doc_str)

  expect_identical(get_doc_field(parsed_fields, "name"), "test's value")
  expect_identical(get_doc_field(parsed_fields, "age"), "2024")
  expect_identical(get_doc_field(parsed_fields, "weight"), "line1 line2")
  expect_true(is.na(get_doc_field(parsed_fields, "missing_field")))
})

test_that("JavaScript parsing helpers handle empty or missing values", {
  expect_length(parse_js_doc_fields("var doc = {};"), 0L)
  expect_identical(normalize_js_field_values(character()), character())
  expect_identical(strip_html_tags_if_present(character()), character())

  title <- parse_title_fields_fast(NA_character_)
  expect_true(is.na(title$primary))
  expect_true(is.na(title$secondary))
  expect_null(parse_js_doc("var doc = { name: \"No user\" };"))
})

test_that("normalize_js_field_values only cleans values that need decoding", {
  raw_values <- c(
    "plain",
    " has spaces ",
    "line1\\\\nline2",
    "quote\\\\\\\"test"
  )

  normalized_values <- normalize_js_field_values(raw_values)

  expect_identical(normalized_values[1], "plain")
  expect_identical(normalized_values[2], "has spaces")
  expect_identical(normalized_values[3], "line1 line2")
  expect_identical(normalized_values[4], "quote\"test")
})

test_that("strip_html_tags_if_present strips only values containing tags", {
  title_values <- c("Plain Text", "<b>Tagged</b> Title")

  stripped_values <- strip_html_tags_if_present(title_values)

  expect_identical(stripped_values[1], "Plain Text")
  expect_identical(stripped_values[2], "Tagged Title")
})

test_that("parse_title_fields_fast strips tags and splits bilingual titles", {
  title_raw <- "Turkce <b>Baslik</b><br/>English <i>Title</i>"
  parsed_title <- parse_title_fields_fast(title_raw)

  expect_identical(parsed_title$primary, "Turkce Baslik")
  expect_identical(parsed_title$secondary, "English Title")
})

test_that("parse_title_fields_fast returns NA secondary for single title", {
  title_raw <- "Tek Baslik"
  parsed_title <- parse_title_fields_fast(title_raw)

  expect_identical(parsed_title$primary, "Tek Baslik")
  expect_true(is.na(parsed_title$secondary))
})


test_that("extract_total_count extracts count from page text", {
  html <- rvest::read_html(mock_search_results_html)
  count <- extract_total_count(html)
  expect_identical(count, 42L)
})

test_that("extract_total_count returns 0 for missing count", {
  html <- rvest::read_html("<html><body>No results info</body></html>")
  count <- extract_total_count(html)
  expect_identical(count, 0L)
})

test_that("extract_total_count returns 0 for unparseable counts", {
  html <- rvest::read_html(
    "<html><body>999999999999999999999 kayıt bulundu</body></html>"
  )

  count <- extract_total_count(html)

  expect_identical(count, 0L)
})

test_that("split_titles separates primary and secondary titles", {
  result <- split_titles("Turkce Baslik / English Title")
  expect_identical(result$primary, "Turkce Baslik")
  expect_identical(result$secondary, "English Title")
})

test_that("split_titles handles single title", {
  result <- split_titles("Only Turkish Title")
  expect_identical(result$primary, "Only Turkish Title")
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
  expect_identical(arabic_result$primary, "التفكر والتأمل عند الصوفية")
  expect_identical(arabic_result$secondary, "Tasavvuf ve zen Budizmi")

  # Chinese thesis: primary=Chinese, secondary=Turkish translation
  chinese_result <- split_titles(
    "老子與古蘭經的比較研究 / Laozi ve Kuran karşılaştırması"
  )
  expect_identical(chinese_result$primary, "老子與古蘭經的比較研究")
  expect_identical(chinese_result$secondary, "Laozi ve Kuran karşılaştırması")

  # Russian thesis: primary=Russian (Cyrillic), secondary=Turkish translation
  russian_result <- split_titles(
    "Лингво-стилистические особенности / Tuzak kelimelerin özellikleri"
  )
  expect_identical(russian_result$primary, "Лингво-стилистические особенности")
  expect_identical(russian_result$secondary, "Tuzak kelimelerin özellikleri")

  # English thesis: primary=English, secondary=Turkish translation
  english_result <- split_titles(
    "The Malaysian Islamic authorities / Malezya dini kurumları"
  )
  expect_identical(english_result$primary, "The Malaysian Islamic authorities")
  expect_identical(english_result$secondary, "Malezya dini kurumları")

  # Japanese thesis: primary=Japanese, secondary=Turkish translation.
  japanese_result <- split_titles(
    "依頼と誘いに対する断り / Reddetme stratejileri"
  )
  expect_identical(japanese_result$primary, "依頼と誘いに対する断り")
  expect_identical(japanese_result$secondary, "Reddetme stratejileri")

  # Greek thesis: primary=Greek (Greek alphabet), secondary=Turkish translation
  greek_result <- split_titles(
    "H kως κατα την βυζαντινη / Bizans döneminde Kos adası"
  )
  expect_identical(greek_result$primary, "H kως κατα την βυζαντινη")
  expect_identical(greek_result$secondary, "Bizans döneminde Kos adası")

  # Korean thesis: primary=Korean (Hangul script), secondary=Turkish translation
  korean_hangul_result <- split_titles(
    "터키 인 학습자를 위한 한국어 양태부사 / Türk öğrenciler için korece"
  )
  expect_identical(
    korean_hangul_result$primary,
    "터키 인 학습자를 위한 한국어 양태부사"
  )
  expect_identical(
    korean_hangul_result$secondary,
    "Türk öğrenciler için korece"
  )

  # Turkish thesis: primary=Turkish, secondary=English translation
  turkish_result <- split_titles("Türkiye'de hanehalkı / Household in Turkiye")
  expect_identical(turkish_result$primary, "Türkiye'de hanehalkı")
  expect_identical(turkish_result$secondary, "Household in Turkiye")
})

test_that("split_titles handles thesis without translation", {
  # Some theses only have title in original language, no translation
  # Real example: Japanese thesis 708017 by BERNA ARIKAN
  japanese_single <- split_titles(
    "依頼と誘いに対する断りにおけるトルコ人日本語学習者の特徴"
  )
  expect_identical(
    japanese_single$primary,
    "依頼と誘いに対する断りにおけるトルコ人日本語学習者の特徴"
  )
  expect_true(is.na(japanese_single$secondary))

  # Greek thesis without translation: 611709 by NIKOLAOS KONTOGIANNIS
  greek_single <- split_titles(
    "H kως κατα την βυζαντινη περιοδο και την ιπποτοκρατια"
  )
  expect_identical(
    greek_single$primary,
    "H kως κατα την βυζαντινη περιοδο και την ιπποτοκρατια"
  )
  expect_true(is.na(greek_single$secondary))

  # Korean thesis with English title only: 707193 by MERT SABRİ KARAMAN
  # Note: Some theses written in one language may only have metadata in another
  korean_english <- split_titles(
    "The US strategy for Korea and ROK-US mutual defense treaty"
  )
  expect_identical(
    korean_english$primary,
    "The US strategy for Korea and ROK-US mutual defense treaty"
  )
  expect_true(is.na(korean_english$secondary))

  # Korean thesis with Turkish title only: 622416 by HANGYOUN CHO
  # Another variation: Korean thesis with only Turkish metadata
  korean_turkish <- split_titles(
    "Kompleks tedavi açısından incelenen Kore Şaman efsaneleri"
  )
  expect_identical(
    korean_turkish$primary,
    "Kompleks tedavi açısından incelenen Kore Şaman efsaneleri"
  )
  expect_true(is.na(korean_turkish$secondary))

  # English thesis with Turkish title only: 626102 by DURSUN EŞSİZ
  # Another variation: English thesis with only Turkish metadata
  english_turkish <- split_titles(
    "Korece öğrenen Türk öğrenciler için Korece duygu kelimeleri"
  )
  expect_identical(
    english_turkish$primary,
    "Korece öğrenen Türk öğrenciler için Korece duygu kelimeleri"
  )
  expect_true(is.na(english_turkish$secondary))
})

test_that("split_titles keeps slash-separated single-language titles intact", {
  title <- paste0(
    "Aggregation and welfare analysis with mixed ",
    "continuous/discrete choice models"
  )
  result <- split_titles(title)

  expect_identical(
    result$primary,
    title
  )
  expect_true(is.na(result$secondary))
})

test_that("split_titles removes dangling trailing slash from single title", {
  title <- paste0(
    "Aggregation and welfare analysis with mixed ",
    "continuous/discrete choice models"
  )
  result <- split_titles(paste0(title, " /"))

  expect_identical(
    result$primary,
    title
  )
  expect_true(is.na(result$secondary))
})

test_that("parse_detail_page adds original and translation aliases", {
  html <- rvest::read_html(mock_detail_html)
  details <- parse_detail_page(html)

  expect_identical(details$title_original, "Yapay Zeka ile Veri Analizi")
  expect_identical(
    details$title_translation,
    "Artificial Intelligence Data Analysis"
  )
  expect_true(grepl("^Bu tez", details$abstract_original))
  expect_true(grepl("^This thesis", details$abstract_translation))
})

test_that("parse_detail_page maps swapped English abstracts", {
  html <- rvest::read_html(mock_detail_html_english_swapped)
  details <- parse_detail_page(html)

  expect_true(grepl("^This thesis", details$abstract_original))
  expect_true(grepl("^Bu tez", details$abstract_translation))
})

test_that("split_bilingual_subjects separates Turkish and English subjects", {
  result <- split_bilingual_subjects(
    "Bilgisayar Muhendisligi=Computer Engineering"
  )
  expect_identical(result$subject_tr, "Bilgisayar Muhendisligi")
  expect_identical(result$subject_en, "Computer Engineering")
})

test_that("split_bilingual_subjects handles multiple subjects", {
  result <- split_bilingual_subjects(
    "Ekonomi=Economics; Ekonometri=Econometrics"
  )
  expect_identical(result$subject_tr, "Ekonomi; Ekonometri")
  expect_identical(result$subject_en, "Economics; Econometrics")
})

test_that("split_bilingual_subjects handles subject without English", {
  result <- split_bilingual_subjects("Ekonomi")
  expect_identical(result$subject_tr, "Ekonomi")
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
  expect_identical(result$tr, c("Ekonomi", "Ekonometri"))
  expect_identical(result$en, c("Economics", "Econometrics"))
})

test_that("split_advisors separates advisor and co-advisor", {
  result <- split_advisors(
    "PROF. DR. MELEK ACAR ; DO\u00C7. DR. \u0130BRAH\u0130M \u00D6ZMEN"
  )
  expect_identical(result$advisor, "PROF. DR. MELEK ACAR")
  expect_identical(
    result$co_advisor,
    "DO\u00C7. DR. \u0130BRAH\u0130M \u00D6ZMEN"
  )
})

test_that("split_advisors handles single advisor", {
  result <- split_advisors("PROF. DR. HASAN \u015EAH\u0130N")
  expect_identical(result$advisor, "PROF. DR. HASAN \u015EAH\u0130N")
  expect_true(is.na(result$co_advisor))
})

test_that("split_advisors handles NA and empty input", {
  result <- split_advisors(NA_character_)
  expect_true(is.na(result$advisor))
  expect_true(is.na(result$co_advisor))

  result <- split_advisors("")
  expect_true(is.na(result$advisor))
  expect_true(is.na(result$co_advisor))

  result <- split_advisors(";")
  expect_true(is.na(result$advisor))
  expect_true(is.na(result$co_advisor))
})

test_that("split_advisors handles multiple co-advisors", {
  result <- split_advisors("A ; B ; C")
  expect_identical(result$advisor, "A")
  expect_identical(result$co_advisor, "B; C")
})

test_that("extract_td_text returns both td blocks", {
  html <- rvest::read_html(mock_detail_html)
  blocks <- extract_td_text(html)

  expect_identical(
    blocks$tr,
    paste0(
      "Bu tez, yapay zeka yontemlerini kullanarak veri analizi yapmaktadir. ",
      "Anahtar Sozcukler: yapay zeka, veri, analiz"
    )
  )
  expect_identical(
    blocks$en,
    paste0(
      "This thesis uses artificial intelligence methods for data analysis. ",
      "Keywords: artificial intelligence, data, analysis"
    )
  )
})

test_that("derive_keyword_fields maps Turkish and English keyword blocks", {
  result <- derive_keyword_fields(
    "This thesis abstract. Keywords: analysis and results",
    "Türkçe özet. Anahtar Sozcukler: Türkçe anahtar"
  )

  expect_identical(result$keywords_tr, "Türkçe anahtar")
  expect_identical(result$keywords_en, "analysis and results")

  result <- derive_keyword_fields(
    "Türkçe özet. Anahtar Sozcukler: Türkçe anahtar",
    "This thesis abstract. Keywords: analysis and results"
  )
  expect_identical(result$keywords_tr, "Türkçe anahtar")
  expect_identical(result$keywords_en, "analysis and results")
})

test_that("extract_stat_year extracts year from stats text", {
  expect_identical(extract_stat_year("2023 210 s. Turkce"), "2023")
  expect_identical(extract_stat_year("Onaylandi 1999"), "1999")
  expect_true(is.na(extract_stat_year("no year here")))
})

test_that("extract_stat_pages extracts page count", {
  expect_identical(extract_stat_pages("2023 210 s. Turkce"), "210")
  expect_identical(extract_stat_pages("150 s."), "150")
  expect_true(is.na(extract_stat_pages("no pages")))
})

test_that("extract_stat_language extracts language", {
  # Function returns list with tr and en fields
  result <- extract_stat_language("2023 210 s. T\u00FCrk\u00E7e")
  expect_identical(result$tr, "T\u00FCrk\u00E7e")
  expect_identical(result$en, "Turkish")

  result <- extract_stat_language("\u0130ngilizce")
  expect_identical(result$tr, "\u0130ngilizce")
  expect_identical(result$en, "English")

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

  expect_identical(first_match$tr, "T\u00FCrk\u00E7e")
  expect_identical(first_match$en, "Turkish")
  expect_identical(second_match$tr, "\u0130ngilizce")
  expect_identical(second_match$en, "English")
})

test_that("extract_bilingual_lookup caches text results by key", {
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
  expect_identical(first_match$tr, second_match$tr)
  expect_identical(first_match$en, second_match$en)
})

test_that("bilingual lookup cache helpers handle invalid keys", {
  reference_data <- tibble::tibble(
    label_tr = "Türkçe",
    label_en = "Turkish"
  )

  metadata <- get_bilingual_lookup_metadata(reference_data, cache_key = NULL)
  result_cache <- get_bilingual_lookup_result_cache(NULL, "Türkçe")

  expect_identical(metadata$label_tr, "Türkçe")
  expect_null(result_cache$key)
  expect_null(result_cache$result)
})

test_that("clearing bilingual lookup cache removes result entries by prefix", {
  result_key <- build_bilingual_lookup_result_key(
    "parse_clear_cache",
    "sample text"
  )
  bilingual_lookup_result_cache[[result_key]] <- list(
    tr = "Türkçe",
    en = "Turkish"
  )

  clear_bilingual_lookup_result_cache("parse_clear_cache")

  expect_false(exists(
    result_key,
    envir = bilingual_lookup_result_cache,
    inherits = FALSE
  ))
  expect_invisible(clear_bilingual_lookup_result_cache(NULL))
})

test_that("coalesce_missing handles NULL, NA, and empty strings", {
  coalesce_missing <- get("coalesce_missing", envir = asNamespace("tezr"))

  expect_identical(coalesce_missing(NULL, "default"), "default")
  expect_identical(coalesce_missing(NA, "default"), "default")
  expect_identical(coalesce_missing("", "default"), "default")
  expect_identical(coalesce_missing("value", "default"), "value")
  expect_identical(coalesce_missing(0, "default"), 0)
})

test_that("extract_keywords_from_index handles mixed bilingual entries", {
  # Mixed: some entries have English translations, some do not
  mixed_index <- "yapay zeka=artificial intelligence; veri analizi"
  result <- extract_keywords_from_index(mixed_index)

  # Only bilingual entries (with =) are included
  expect_identical(result$tr, "yapay zeka")
  expect_identical(result$en, "artificial intelligence")
})

test_that("extract_keywords_from_index returns NA for empty input", {
  result <- extract_keywords_from_index(NA_character_)
  expect_true(is.na(result$tr))
  expect_true(is.na(result$en))

  result2 <- extract_keywords_from_index("")
  expect_true(is.na(result2$tr))
  expect_true(is.na(result2$en))
})

test_that("extract_keywords returns parsed keywords from td text", {
  html <- rvest::read_html("<html><body></body></html>")
  td_text <- list(
    tr = "Özet metni. Anahtar Sozcukler: enerji, üretim",
    en = NA_character_
  )

  result <- extract_keywords(html, "tr", td_text = td_text)

  expect_identical(result, "enerji, üretim")
})

test_that("merge_keywords deduplicates case-insensitively", {
  result <- merge_keywords("Yapay Zeka; veri", "yapay zeka; analiz")

  keywords <- strsplit(result, "; ", fixed = TRUE)[[1]]
  # Should have 3 unique keywords (Yapay Zeka, veri, analiz)
  expect_length(keywords, 3L)
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
  expect_identical(result, "open")
})

test_that("extract_access_status returns restricted for restricted text", {
  html <- rvest::read_html("<html><body>\u0130zinsiz</body></html>")
  result <- extract_access_status(html, pdf_link = NULL)
  expect_identical(result, "restricted")
})

test_that("extract_access_status returns NA for unknown", {
  html <- rvest::read_html("<html><body>Nothing relevant</body></html>")
  result <- extract_access_status(html, pdf_link = NULL)
  expect_true(is.na(result))
})
