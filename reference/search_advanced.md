# Advanced search of the Turkiye's National Thesis Center

Keyword-based search with common filter options. Similar to basic search
but adds year filtering, language, university, institute, group, and
status filters. Limited to 2000 results per request (server limit). For
more results or field-specific searches, use
[`search_detailed`](https://eremrah.com/tezr/reference/search_detailed.md).

## Usage

``` r
search_advanced(
  keyword,
  search_field = c("title", "all", "author", "supervisor", "subject", "index",
    "abstract"),
  match_type = c("exact", "contains"),
  year_start = NULL,
  year_end = NULL,
  group = c("all", "science", "social", "medical"),
  university = NULL,
  university_id = NULL,
  thesis_type = c("all", "masters", "phd", "medical_specialty", "arts", "dentistry",
    "medical_sub", "pharmacy"),
  institute = NULL,
  institute_id = NULL,
  language = NULL,
  access_type = c("all", "open", "restricted"),
  status = c("approved", "all", "in_preparation"),
  max_search_results = 2000,
  ignore_cache = FALSE
)
```

## Arguments

- keyword:

  Character. A single search term.

- search_field:

  Character. The field to search in. One of "title", "author",
  "supervisor", "subject", "index", "abstract", or "all". Default is
  "title" (matches the web advanced search form).

- match_type:

  Character. Keyword matching strategy. One of "exact" (matches the
  keyword as entered) or "contains" (substring match). Default is
  "exact".

- year_start:

  Integer. Start year (optional).

- year_end:

  Integer. End year (optional).

- group:

  Character. Group filter. One of "all", "science", "social", or
  "medical". Default is "all".

- university:

  Character. University name (optional). If provided without
  `university_id`, the ID is looked up automatically.

- university_id:

  Integer. University ID (optional). Use this to skip lookup.

- thesis_type:

  Character. Type of thesis. One of "all", "masters", "phd",
  "medical_specialty", "arts", "dentistry", "medical_sub", "pharmacy".
  Default is "all".

- institute:

  Character. Institute name (optional). If provided without
  `institute_id`, the ID is looked up automatically. Institute filters
  require a field-specific `search_field`.

- institute_id:

  Integer. Institute ID (optional). Use this to skip lookup. Institute
  filters require a field-specific `search_field`.

- language:

  Integer language ID or character label (e.g., "tr", "en", "Turkish",
  "İngilizce").

- access_type:

  Character. Access type. One of "all", "open", "restricted". Default is
  "all".

- status:

  Character. Thesis status. One of "all", "approved", "in_preparation".
  Default is "approved".

- max_search_results:

  Maximum results to return. Default is 2000 (server limit per query).
  Use higher values or `Inf` to paginate and retrieve more results via
  year-range splitting.

- ignore_cache:

  Logical. If `TRUE`, bypass cached search/range results and fetch fresh
  data from the server.

## Value

A tibble containing thesis records (same structure as search_basic).

## Details

The YOK portal's advanced search form supports up to three keyword rows
combined with Boolean operators (AND, OR, NOT), each targeting a
different field. This function exposes only the first keyword row. R
packages that interface with academic databases, such as
[rentrez](https://docs.ropensci.org/rentrez/) (PubMed) and
[europepmc](https://docs.ropensci.org/europepmc/) (Europe PMC), pass
Boolean logic as a single query string (e.g., `"term1 AND term2"`). The
YOK portal does not accept free-form Boolean strings; it uses structured
form fields for each keyword row, making that pattern inapplicable here.
University and group filters are sent with the keyword endpoint.
Institute filters are sent through the detailed form for field-specific
searches because YOK's all-field keyword endpoint ignores institute
values.

For equivalent results:

- **AND across fields**: use
  [`search_detailed`](https://eremrah.com/tezr/reference/search_detailed.md)
  with its field-specific parameters (`title`, `author`, `supervisor`,
  etc.).

- **OR**: run separate searches and combine with
  `dplyr::bind_rows() |> dplyr::distinct()`.

- **NOT**: run both searches and exclude with
  [`dplyr::anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html).

## Examples

``` r
if (FALSE) { # \dontrun{
# Keyword search with year filter
climate <- search_advanced(
  keyword = "iklim değişikliği",
  year_start = 2015
)

# Search PhD theses only
ml <- search_advanced(
  keyword = "makine öğrenmesi",
  thesis_type = "phd"
)

# Search in-preparation theses
ongoing <- search_advanced(
  keyword = "ekonometri",
  status = "in_preparation"
)

# Paginate a broad query
all_climate <- search_advanced(
  keyword = "iklim değişikliği",
  max_search_results = Inf
)
} # }
```
