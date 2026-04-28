test_that("field codes are correct", {
  expect_equal(search_field_codes$title, 1L)
  expect_equal(search_field_codes$author, 2L)
  expect_equal(search_field_codes$supervisor, 3L)
  expect_equal(search_field_codes$all, 7L)
})

test_that("thesis type codes are correct", {
  expect_equal(thesis_type_codes$all, 0L)
  expect_equal(thesis_type_codes$masters, 1L)
  expect_equal(thesis_type_codes$phd, 2L)
})

test_that("build_basic_search_form creates correct structure", {
  form <- build_basic_search_form(
    keyword = "test",
    search_field = "title",
    thesis_type = "phd",
    access_type = "open"
  )

  expect_type(form, "list")
  expect_equal(form$neden, "test")
  expect_equal(form$nevi, 1L)
  expect_equal(form$tur, 2L)
  expect_equal(form$izin, 1L)
  expect_equal(form$islem, 1L)
})

test_that("build_advanced_search_form produces correct field count and values", {
  form <- build_advanced_search_form(
    keyword = "test keyword",
    search_field = "title",
    thesis_type = "phd",
    access_type = "open",
    status = "in_preparation"
  )

  expect_type(form, "list")
  expect_equal(form$keyword, "test keyword")
  expect_equal(form$nevi, 1L)
  expect_equal(form$Tur, 2L)
  expect_equal(form$izin, 1L)
  expect_equal(form$Durum, 1L) # in_preparation = 1
  expect_equal(form$islem, 4L)
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
  expect_equal(form_exact$tip, 1L)

  form_contains <- build_advanced_search_form(
    keyword = "test",
    search_field = "all",
    thesis_type = "all",
    access_type = "all",
    match_type = "contains"
  )
  expect_equal(form_contains$tip, 2L)
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

  expect_equal(form$uniad, "Test Uni")
  expect_equal(form$Universite, 123L)
  expect_equal(form$ensad, "Test Inst")
  expect_equal(form$Enstitu, 456L)
})

test_that("build_detailed_search_form respects status parameter", {
  form_approved <- build_detailed_search_form(
    author = "test",
    status = "approved"
  )
  expect_equal(form_approved$Durum, 3L)

  form_prep <- build_detailed_search_form(
    author = "test",
    status = "in_preparation"
  )
  expect_equal(form_prep$Durum, 1L)

  form_all <- build_detailed_search_form(author = "test", status = "all")
  expect_equal(form_all$Durum, 0L)
})

test_that("clean_text works correctly", {
  expect_equal(clean_text("  hello  world  "), "hello world")
  expect_equal(clean_text("test"), "test")
  expect_equal(clean_text("line1\tline2"), "line1 line2")
})

test_that("clean_advisor_name strips various Turkish academic titles", {
  expect_equal(clean_advisor_name("Prof. Dr. Ahmet Yılmaz"), "AHMET YILMAZ")
  expect_equal(clean_advisor_name("Dr. Mehmet Demir"), "MEHMET DEMİR")
})

test_that("clean_advisor_name handles NULL and empty input", {
  expect_equal(clean_advisor_name(NULL), "")
  expect_equal(clean_advisor_name(""), "")
})

test_that("validate_year works correctly", {
  expect_null(validate_year(NULL))
  expect_equal(validate_year(2020), 2020L)
  expect_equal(validate_year("2020"), 2020L)
  expect_error(validate_year(c(2020, 2021)), "single")
  expect_error(validate_year(1800))
  expect_error(validate_year(2200))
})

test_that("validate_year at boundary values", {
  expect_equal(validate_year(1959), 1959L)
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  expect_equal(validate_year(current_year), current_year)
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

  expect_equal(basic_tr$Dil, tr_id)
  expect_equal(detailed_en$Dil, en_id)
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
  expect_equal(resolve_language_id(tr_id), tr_id)
  expect_equal(resolve_language_id(as.character(tr_id)), tr_id)
  expect_equal(resolve_language_id("tr"), tr_id)
  expect_error(resolve_language_id("klingon"), "language")
})

test_that("resolve_language_id accepts ISO 639 codes for all languages", {
  fr_id <- as.integer(languages$value[languages$label_en == "French"][1])
  de_id <- as.integer(languages$value[languages$label_en == "German"][1])
  ja_id <- as.integer(languages$value[languages$label_en == "Japanese"][1])
  expect_equal(resolve_language_id("fr"), fr_id)
  expect_equal(resolve_language_id("de"), de_id)
  expect_equal(resolve_language_id("ja"), ja_id)
})

test_that("normalize_language_label strips Turkish diacritics", {
  expect_equal(normalize_language_label("İngilizce"), "ingilizce")
  expect_equal(normalize_language_label("Türkçe"), "turkce")
})

test_that("coerce_search_fields normalizes years and language", {
  tr_id <- as.integer(languages$value[languages$label_en == "Turkish"][1])
  coerced <- coerce_search_fields(
    year_start = "2020",
    year_end = 2021,
    language = "tr"
  )

  expect_equal(coerced$year_start, 2020L)
  expect_equal(coerced$year_end, 2021L)
  expect_equal(coerced$language_id, tr_id)
})

test_that("coerce_search_fields fills in missing year_start when year_end provided", {
  coerced <- coerce_search_fields(year_start = NULL, year_end = 2020)
  expect_equal(coerced$year_start, 1959L)
  expect_equal(coerced$year_end, 2020L)
})

test_that("coerce_search_fields fills in missing year_end when year_start provided", {
  coerced <- coerce_search_fields(year_start = 2020, year_end = NULL)
  expect_equal(coerced$year_start, 2020L)
  expect_equal(coerced$year_end, as.integer(format(Sys.Date(), "%Y")))
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

  expect_equal(form$uniad, "Test Uni")
  expect_equal(form$Universite, 123L)
  expect_equal(form$ensad, "Test Inst")
  expect_equal(form$Enstitu, 456L)
  expect_equal(form$abdad, "Test Division")
  expect_equal(form$ABD, 789L)
  expect_equal(form$bilim, "Test Discipline")
  expect_equal(form$BilimDali, 101112L)
  expect_equal(form$Konu, "Test Subject")
  expect_equal(form$EnstituGrubu, group_codes$social)
  # Status default is approved = 3
  expect_equal(form$Durum, 3L)
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
