# Search the Turkiye's National Thesis Center

Searches the Turkiye's National Thesis Center (Ulusal Tez Merkezi)
database and returns matching thesis records.

## Usage

``` r
search_basic(
  keyword,
  search_field = c("all", "title", "author", "supervisor", "subject", "index",
    "abstract", "thesis_no"),
  access_type = c("all", "open", "restricted"),
  thesis_type = c("all", "masters", "phd", "medical_specialty", "arts", "dentistry",
    "medical_sub", "pharmacy"),
  max_search_results = 2000,
  ignore_cache = FALSE
)
```

## Arguments

- keyword:

  Character. The search term(s).

- search_field:

  Character. The field to search in. One of "title", "author",
  "supervisor", "subject", "index", "abstract", "all", or "thesis_no".
  Default is "all".

- access_type:

  Character. Access type filter. One of "all", "open", or "restricted".
  Default is "all".

- thesis_type:

  Character. Type of thesis to search for. One of "all", "masters",
  "phd", "medical_specialty", "arts", "dentistry", "medical_sub", or
  "pharmacy". Default is "all".

- max_search_results:

  Maximum results to return. Default is 2000 (server limit per query).
  Set to `Inf` to automatically delegate to
  [`search_advanced`](https://eremrah.com/tezr/reference/search_advanced.md)
  for pagination when results exceed 2000.

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

Basic search returns up to 2000 results per query (YOK server limit).
When results exceed 2000 and `max_search_results > 2000`, the search
delegates to
[`search_advanced`](https://eremrah.com/tezr/reference/search_advanced.md)
which paginates via year-range splitting to retrieve all results. With
the default `max_search_results = 2000`, a warning is issued suggesting
`max_search_results = Inf`.

## See also

Other search functions:
[`search_advanced()`](https://eremrah.com/tezr/reference/search_advanced.md),
[`search_detailed()`](https://eremrah.com/tezr/reference/search_detailed.md)

## Examples

``` r
if (FALSE) { # interactive()
# Search for theses (returns up to 2000 results)
search_results <- search_basic("hanehalkı")

# Search for PhD theses by a specific author
search_results <- search_basic(
  "Nilay Ünsal",
  search_field = "author",
  thesis_type = "masters"
)

# Search for open access theses
search_results <- search_basic("tarım", access_type = "open")

# Get all results (auto-delegates to advanced search for pagination)
all_results <- search_basic("hanehalkı", max_search_results = Inf)
}
```
