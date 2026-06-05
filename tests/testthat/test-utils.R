test_that("field codes are correct", {
  expect_identical(search_field_codes$title, 1L)
  expect_identical(search_field_codes$author, 2L)
  expect_identical(search_field_codes$supervisor, 3L)
  expect_identical(search_field_codes$all, 7L)
})

test_that("thesis type codes are correct", {
  expect_identical(thesis_type_codes$all, 0L)
  expect_identical(thesis_type_codes$masters, 1L)
  expect_identical(thesis_type_codes$phd, 2L)
})

test_that("build_basic_search_form creates correct structure", {
  form <- build_basic_search_form(
    keyword = "test",
    search_field = "title",
    thesis_type = "phd",
    access_type = "open"
  )

  expect_type(form, "list")
  expect_identical(form$neden, "test")
  expect_identical(form$nevi, 1L)
  expect_identical(form$tur, 2L)
  expect_identical(form$izin, 1L)
  expect_identical(form$islem, 1L)
})

test_that("build_advanced_search_form has expected fields", {
  form <- build_advanced_search_form(
    keyword = "test keyword",
    search_field = "title",
    thesis_type = "phd",
    access_type = "open",
    status = "in_preparation"
  )

  expect_type(form, "list")
  expect_identical(form$keyword, "test keyword")
  expect_identical(form$nevi, 1L)
  expect_identical(form$Tur, 2L)
  expect_identical(form$izin, 1L)
  expect_identical(form$Durum, 1L) # in_preparation = 1
  expect_identical(form$islem, 4L)
  # Should have all expected fields
  expect_true(all(
    c("keyword", "nevi", "Tur", "izin", "Durum", "islem") %in% names(form)
  ))
})

test_that("build_advanced_search_form respects match_type parameter", {
  form_exact <- build_advanced_search_form(
    keyword = "test",
    search_field = "all",
    thesis_type = "all",
    access_type = "all",
    match_type = "exact"
  )
  expect_identical(form_exact$tip, 1L)

  form_contains <- build_advanced_search_form(
    keyword = "test",
    search_field = "all",
    thesis_type = "all",
    access_type = "all",
    match_type = "contains"
  )
  expect_identical(form_contains$tip, 2L)
})

test_that("build_advanced_search_form handles university/institute IDs", {
  form <- build_advanced_search_form(
    keyword = "test",
    search_field = "all",
    thesis_type = "all",
    access_type = "all",
    university = "Test Uni",
    university_id = "123",
    institute = "Test Inst",
    institute_id = 456
  )

  expect_identical(form$uniad, "Test Uni")
  expect_identical(form$Universite, 123L)
  expect_identical(form$ensad, "Test Inst")
  expect_identical(form$Enstitu, 456L)
})

test_that("build_detailed_search_form respects status parameter", {
  form_approved <- build_detailed_search_form(
    author = "test",
    status = "approved"
  )
  expect_identical(form_approved$Durum, 3L)

  form_prep <- build_detailed_search_form(
    author = "test",
    status = "in_preparation"
  )
  expect_identical(form_prep$Durum, 1L)

  form_all <- build_detailed_search_form(author = "test", status = "all")
  expect_identical(form_all$Durum, 0L)
})

test_that("clean_text works correctly", {
  expect_identical(clean_text(character()), character())
  expect_identical(clean_text("  hello  world  "), "hello world")
  expect_identical(clean_text("test"), "test")
  expect_identical(clean_text("line1\tline2"), "line1 line2")
})

test_that("clean_advisor_name strips various Turkish academic titles", {
  expect_identical(clean_advisor_name("Prof. Dr. Ahmet Yılmaz"), "AHMET YILMAZ")
  expect_identical(clean_advisor_name("Dr. Mehmet Demir"), "MEHMET DEMİR")
})

test_that("clean_advisor_name handles NULL and empty input", {
  expect_identical(clean_advisor_name(NULL), "")
  expect_identical(clean_advisor_name(""), "")
})

test_that("validate_year works correctly", {
  expect_null(validate_year(NULL))
  expect_identical(validate_year(2020), 2020L)
  expect_identical(validate_year("2020"), 2020L)
  expect_error(validate_year(c(2020, 2021)), "single")
  expect_error(validate_year("bad"), "valid year")
  expect_error(validate_year(1800))
  expect_error(validate_year(2200))
})

test_that("validate_year at boundary values", {
  expect_identical(validate_year(1959), 1959L)
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  expect_identical(validate_year(current_year), current_year)
  expect_error(validate_year(1958), "between 1959")
  expect_error(validate_year(current_year + 1), "between 1959")
})

test_that("search forms resolve language labels", {
  tr_id <- as.integer(languages$value[languages$label_en == "Turkish"][1])
  en_id <- as.integer(languages$value[languages$label_en == "English"][1])

  basic_tr <- build_basic_search_form(
    keyword = "test",
    search_field = "title",
    thesis_type = "phd",
    access_type = "open",
    language = "tr"
  )
  detailed_en <- build_detailed_search_form(language = "English")

  expect_identical(basic_tr$Dil, tr_id)
  expect_identical(detailed_en$Dil, en_id)
  expect_error(
    build_basic_search_form(
      keyword = "test",
      search_field = "title",
      thesis_type = "phd",
      access_type = "open",
      language = "klingon"
    ),
    "language"
  )
})

test_that("resolve_language_id handles ids and labels", {
  tr_id <- as.integer(languages$value[languages$label_en == "Turkish"][1])
  expect_identical(resolve_language_id(tr_id), tr_id)
  expect_identical(resolve_language_id(as.character(tr_id)), tr_id)
  expect_identical(resolve_language_id("tr"), tr_id)
  expect_error(resolve_language_id("klingon"), "language")
})

test_that("resolve_language_id accepts ISO 639 codes for all languages", {
  fr_id <- as.integer(languages$value[languages$label_en == "French"][1])
  de_id <- as.integer(languages$value[languages$label_en == "German"][1])
  ja_id <- as.integer(languages$value[languages$label_en == "Japanese"][1])
  expect_identical(resolve_language_id("fr"), fr_id)
  expect_identical(resolve_language_id("de"), de_id)
  expect_identical(resolve_language_id("ja"), ja_id)
})

test_that("normalize_language_label strips Turkish diacritics", {
  expect_identical(normalize_language_label("İngilizce"), "ingilizce")
  expect_identical(normalize_language_label("Türkçe"), "turkce")
  expect_true(is.na(normalize_language_label(NULL)))
})

test_that("coerce_search_fields normalizes years and language", {
  tr_id <- as.integer(languages$value[languages$label_en == "Turkish"][1])
  coerced <- coerce_search_fields(
    year_start = "2020",
    year_end = 2021,
    language = "tr"
  )

  expect_identical(coerced$year_start, 2020L)
  expect_identical(coerced$year_end, 2021L)
  expect_identical(coerced$language_id, tr_id)
})

test_that("coerce_search_fields fills missing year_start", {
  coerced <- coerce_search_fields(year_start = NULL, year_end = 2020)
  expect_identical(coerced$year_start, 1959L)
  expect_identical(coerced$year_end, 2020L)
})

test_that("coerce_search_fields fills missing year_end", {
  coerced <- coerce_search_fields(year_start = 2020, year_end = NULL)
  expect_identical(coerced$year_start, 2020L)
  expect_identical(coerced$year_end, as.integer(format(Sys.Date(), "%Y")))
})

test_that("build_detailed_search_form handles IDs correctly", {
  form <- build_detailed_search_form(
    university = "Test Uni",
    university_id = "123",
    institute = "Test Inst",
    institute_id = 456,
    division = "Test Division",
    division_id = "789",
    discipline = "Test Discipline",
    discipline_id = "101112",
    subject = "Test Subject",
    subject_id = "131415",
    group = "social"
  )

  expect_identical(form$uniad, "Test Uni")
  expect_identical(form$Universite, 123L)
  expect_identical(form$ensad, "Test Inst")
  expect_identical(form$Enstitu, 456L)
  expect_identical(form$abdad, "Test Division")
  expect_identical(form$ABD, 789L)
  expect_identical(form$bilim, "Test Discipline")
  expect_identical(form$BilimDali, 101112L)
  expect_identical(form$Konu, "Test Subject")
  expect_identical(form$EnstituGrubu, group_codes$social)
  # Status default is approved = 3
  expect_identical(form$Durum, 3L)
})

test_that("form field helpers coerce optional values", {
  expect_identical(form_text(NULL), "")
  expect_identical(form_text("abc"), "abc")
  expect_identical(form_text(123, as.character), "123")

  expect_identical(form_int(NULL), 0L)
  expect_identical(form_int("123"), 123L)
})

test_that("language helper paths validate ids and aliases", {
  tr_id <- as.integer(languages$value[languages$label_en == "Turkish"][1])

  expect_identical(resolve_numeric_language_id(tr_id), tr_id)
  expect_error(resolve_numeric_language_id("not-an-id"), "valid language id")
  expect_error(resolve_numeric_language_id(999999), "valid language id")
  expect_error(
    testthat::with_mocked_bindings(
      match_language_label_id("turkish"),
      languages = tibble::tibble(
        label_tr = "Türkçe",
        label_en = "Turkish",
        value = NA_character_
      ),
      .package = "tezr"
    ),
    "valid language id"
  )
  expect_null(resolve_character_language_id(""))
  expect_null(testthat::with_mocked_bindings(
    resolve_character_language_id("English"),
    normalize_language_label = function(...) NA_character_,
    .package = "tezr"
  ))
  expect_identical(resolve_character_language_id(as.character(tr_id)), tr_id)
  expect_identical(resolve_language_alias("tr"), "turkish")
  expect_identical(resolve_language_alias("turkish"), "turkish")
  expect_null(resolve_language_id(NULL))
  expect_error(resolve_language_id(c("tr", "en")), "single value")
  expect_error(resolve_language_id(TRUE), "numeric id or character label")
})

test_that("optional label validation trims and rejects empty values", {
  expect_identical(
    validate_optional_label("  Ankara  ", "university"),
    "Ankara"
  )
  expect_null(validate_optional_label(NULL, "university"))
  expect_error(validate_optional_label("", "university"), "non-empty")
  expect_error(validate_optional_label(c("a", "b"), "university"), "non-empty")
})

test_that("bilingual and cache validators handle empty values", {
  parsed <- parse_bilingual_entries(" ; ")
  expect_identical(parsed$tr, character())
  expect_identical(parsed$en, character())

  expect_error(validate_ignore_cache(NA), "TRUE or FALSE")
})

test_that("sysdata only includes used tables", {
  sysdata_env <- new.env(parent = emptyenv())
  # During R CMD check the source tree layout differs, so try system.file first
  sysdata_path <- system.file("R", "sysdata.rda", package = "tezr")
  if (nchar(sysdata_path) == 0) {
    sysdata_path <- testthat::test_path("..", "..", "R", "sysdata.rda")
  }
  skip_if_not(file.exists(sysdata_path), "sysdata.rda not found")
  load(sysdata_path, envir = sysdata_env)

  expect_true(all(c("thesis_types", "languages") %in% ls(sysdata_env)))
  expect_false("access_types" %in% ls(sysdata_env))
  expect_false("groups" %in% ls(sysdata_env))
})
