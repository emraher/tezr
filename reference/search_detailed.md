# Detailed search of the Turkiye's National Thesis Center

Searches with detailed field-specific filter options. When total results
exceed 2000 (server limit), automatically paginates using year ranges to
retrieve all results.

## Usage

``` r
search_detailed(
  university = NULL,
  university_id = NULL,
  thesis_type = "all",
  year_start = NULL,
  year_end = NULL,
  institute = NULL,
  institute_id = NULL,
  access_type = "all",
  group = "all",
  thesis_no = NULL,
  division = NULL,
  division_id = NULL,
  status = "approved",
  title = NULL,
  discipline = NULL,
  discipline_id = NULL,
  language = NULL,
  author = NULL,
  subject = NULL,
  supervisor = NULL,
  keyword = NULL,
  abstract = NULL,
  max_search_results = 2000,
  ignore_cache = FALSE
)
```

## Arguments

- university:

  University name (optional). Accepts character vector for multiple
  universities.

- university_id:

  University ID (optional). If provided, lookup by `university` is
  skipped.

- thesis_type:

  Type(s) of thesis. Default is "all". Accepts character vector for
  multiple types: "all", "masters", "phd", "medical_specialty", "arts",
  "dentistry", "medical_sub", "pharmacy". Multiple values will trigger
  separate searches that are combined.

- year_start:

  Start year (optional). Used for pagination when results \> 2000.

- year_end:

  End year (optional). Used for pagination when results \> 2000.

- institute:

  Institute name (optional). Accepts character vector for multiple
  institutes.

- institute_id:

  Institute ID (optional). If provided, lookup by `institute` is
  skipped.

- access_type:

  Access type. Default is "all". Accepts character vector for multiple
  access types: "all", "open", "restricted". Multiple values will
  trigger separate searches that are combined.

- group:

  Group filter. One of "all", "science", "social", or "medical". Default
  is "all".

- thesis_no:

  Thesis number to search for (optional). Accepts character vector for
  multiple thesis numbers.

- division:

  Division name (optional). Accepts character vector for multiple
  divisions.

- division_id:

  Division ID (optional). If provided, lookup by `division` is skipped.

- status:

  Character. Thesis status filter. One of "approved", "all",
  "in_preparation". Default is "approved".

- title:

  Title to search for (optional). Accepts character vector for multiple
  titles.

- discipline:

  Discipline name (optional). Accepts character vector for multiple
  disciplines.

- discipline_id:

  Discipline ID (optional). If provided, lookup by `discipline` is
  skipped.

- language:

  Integer language ID or character label (e.g., "tr", "en", "Turkish",
  "İngilizce"). Accepts a character vector for multiple languages.

- author:

  Author name (optional). Accepts character vector for multiple authors.

- subject:

  Subject (Konu) name (optional). Accepts character vector for multiple
  subjects.

- supervisor:

  Supervisor name (optional). Accepts character vector for multiple
  supervisors.

- keyword:

  Keyword text (Dizin) to search (optional). Accepts character vector
  for multiple keywords.

- abstract:

  Abstract text to search (optional). Accepts character vector for
  multiple abstracts.

- max_search_results:

  Maximum results to return. Default is 2000 (server limit per query).
  Use higher values or `Inf` to get all available results via automatic
  pagination.

- ignore_cache:

  Logical. If `TRUE`, bypass cached search/range results and fetch fresh
  data from the server.

## Value

A tibble containing thesis records with columns:

- thesis_no - Unique thesis identifier

- title_original - Original title

- title_translation - Title translation when available

- author - Author name

- university - University name

- year - Year of thesis

- thesis_type_tr - Type of thesis in Turkish

- thesis_type_en - Type of thesis in English

- language_tr - Thesis language in Turkish

- language_en - Thesis language in English

- subject_tr - Turkish subject classification

- subject_en - English subject classification

- detail_id - ID for fetching details

## Details

The YOK web portal accepts only a single value per filter field. This
function extends beyond the portal by accepting vector-valued parameters
for most fields. Multiple values are expanded into separate API calls,
and the results are combined and deduplicated by thesis number.

When the total result count exceeds 2000 and no year range is specified,
the function automatically uses 1959-present as the year range and
paginates to retrieve all results.

## See also

Other search functions:
[`search_advanced()`](https://eremrah.com/tezr/reference/search_advanced.md),
[`search_basic()`](https://eremrah.com/tezr/reference/search_basic.md)

## Examples

``` r
if (FALSE) { # interactive()
# Field-specific search with year filter
climate <- search_detailed(
  title = "iklim değişikliği",
  year_start = 2015,
  year_end = 2024
)

# Title search with university filter
ml_theses <- search_detailed(
  title = "makine öğrenmesi",
  university = "Orta Doğu Teknik Üniversitesi",
  thesis_type = "phd"
)

# Search by university and division
search_results <- search_detailed(
  university = "Ankara Üniversitesi",
  division = "İktisat Ana Bilim Dalı",
  year_start = 2020,
  year_end = 2023
)

# Get ALL results for a subject (auto-paginates if > 2000)
all_econ <- search_detailed(subject = "Ekonometri", max_search_results = Inf)

# Search for a specific supervisor's theses
search_results <- search_detailed(supervisor = "Hasan Şahin")

# Search across multiple universities (automatically expands)
search_results <- search_detailed(
  university = c("Ankara Üniversitesi", "İstanbul Üniversitesi"),
  subject = "Ekonomi"
)

# Search multiple disciplines within a subject
search_results <- search_detailed(
  subject = "Ekonomi",
  discipline = c("İktisat", "Maliye", "Ekonometri")
)

# Search for multiple specific thesis numbers
search_results <- search_detailed(
  thesis_no = c("123456", "234567", "345678")
)

# Search for multiple titles
search_results <- search_detailed(
  title = c("Ekonometri", "Zaman Serileri")
)

# Search for multiple languages
search_results <- search_detailed(
  subject = "Ekonomi",
  language = c("tr", "en")
)

# Search for multiple thesis types
search_results <- search_detailed(
  subject = "Ekonomi",
  thesis_type = c("phd", "masters")
)

# Search for multiple access types
search_results <- search_detailed(
  subject = "Makine Mühendisliği",
  access_type = c("open", "restricted")
)

# Complex multi-parameter search
search_results <- search_detailed(
  author = c("Ahmet Yılmaz", "Mehmet Demir"),
  university = c("Ankara Üniversitesi", "İstanbul Üniversitesi"),
  year_start = 2020
)
}
```
