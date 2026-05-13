# Detailed search of the Turkiye's National Thesis Center

Searches YOK's redesigned detailed form. When total results exceed 2000,
automatically paginates using year ranges to retrieve more results. Very
broad single-year ranges can still be capped by the server.

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

  University name filter. If provided without `university_id`, the ID is
  looked up automatically.

- university_id:

  University ID filter. Use this to skip lookup.

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

  Institute name filter. If provided without `institute_id`, the ID is
  looked up automatically.

- institute_id:

  Institute ID filter. Use this to skip lookup.

- access_type:

  Access type. Default is "all". Accepts character vector for multiple
  access types: "all", "open", "restricted". Multiple values will
  trigger separate searches that are combined.

- group:

  Group filter. One of "all", "science", "social", or "medical".

- thesis_no:

  Thesis number to search for. This uses YOK's detailed form endpoint
  because the redesigned keyword endpoint is unreliable for thesis
  numbers.

- division:

  Division name filter. If provided without `division_id`, the ID is
  looked up automatically.

- division_id:

  Division ID filter. Use this to skip lookup.

- status:

  Character. Thesis status filter. One of "approved", "all",
  "in_preparation". Default is "approved".

- title:

  Title to search for (optional). Accepts character vector for multiple
  titles.

- discipline:

  Discipline name filter. If provided without `discipline_id`, the ID is
  looked up automatically.

- discipline_id:

  Discipline ID filter. Use this to skip lookup.

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
  Use higher values or `Inf` to paginate beyond the first server batch
  when year-range splitting can narrow each request below the server
  cap.

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

The detailed form accepts field-specific and institutional filters in
the same request, including `title`, `author`, `supervisor`, `subject`,
`keyword`, `abstract`, `thesis_no`, `university`, `institute`,
`division`, `discipline`, and `group`. Vector-valued fields are expanded
into separate requests, and the results are combined and deduplicated by
thesis number.

## Examples

``` r
if (FALSE) { # \dontrun{
# Field-specific search with year filter
climate <- search_detailed(
  title = "iklim değişikliği",
  year_start = 2015,
  year_end = 2024
)

# Title search with a thesis type filter
ml_theses <- search_detailed(
  title = "makine öğrenmesi",
  thesis_type = "phd"
)

# Search by subject and year range
search_results <- search_detailed(
  subject = "Ekonometri",
  year_start = 2020,
  year_end = 2023
)

# Paginate a broad subject search
all_econ <- search_detailed(subject = "Ekonometri", max_search_results = Inf)

# Search for a specific supervisor's theses
search_results <- search_detailed(supervisor = "Hasan Şahin")

# Search for a specific thesis number
search_results <- search_detailed(thesis_no = "12345")

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
  year_start = 2020
)
} # }
```
