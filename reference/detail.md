# Get detailed information about a thesis

Retrieves the full details of one or more theses from the Turkish
National Thesis Center using encoded IDs from search results.

## Usage

``` r
detail(detail_id, progress = TRUE, encrypted_no = NULL, ...)
```

## Arguments

- detail_id:

  Character vector, detail URL, or search-result data frame. Character
  values may be encoded IDs from the `detail_id` column or redesigned
  YOK detail URLs. A data frame returned by a search function can be
  passed directly. Accepts multiple values or rows for batch retrieval.

- progress:

  Logical. Show text progress updates when fetching multiple theses?
  Default is TRUE.

- encrypted_no:

  Character vector. Optional encrypted thesis number from the
  `encrypted_no` column of redesigned search results. When available,
  `detail()` includes it in the YOK detail request and uses the JSON
  detail endpoint to add citation metadata.

- ...:

  Reserved for internal use.

## Value

A tibble with thesis details (one row per thesis). Columns (in order):

- thesis_no - Thesis number

- title_original - Original title

- title_translation - Title translation when available

- author - Author name

- advisor - Advisor name and title

- co_advisor - Co-advisor name(s) (semicolon-separated if multiple; NA
  when absent)

- university - University name

- institute - Institute name

- division - Division name

- year - Year

- pages - Number of pages

- thesis_type_tr - Thesis type in Turkish (e.g., "Doktora", "Yüksek
  Lisans")

- thesis_type_en - Thesis type in English (e.g., "Doctorate",
  "Master's")

- language_tr - Language in Turkish (e.g., "Türkçe", "İngilizce")

- language_en - Language in English (e.g., "Turkish", "English")

- subject_tr - Turkish subject classifications (semicolon-separated when
  multiple)

- subject_en - English subject classifications (semicolon-separated when
  multiple)

- abstract_original - Abstract in the thesis's original language when
  available

- abstract_translation - Translated abstract when available

- keywords_tr - Turkish keywords (includes keywords from index field)

- keywords_en - English keywords (includes keywords from index field)

- access_status - Access status (open/restricted)

- pdf_url - URL to download PDF (if available)

- detail_url - URL to the thesis detail page

- citation_apa, citation_ieee, citation_mla, citation_chicago,
  citation_harvard - Citation strings when YOK's JSON detail endpoint is
  available

## Examples

``` r
if (FALSE) { # \dontrun{
# Search for theses
results <- search_basic("panel veri")

# Get details for a single thesis, including citation metadata when present
thesis_details <- detail(results[1, ])

# Get details for multiple theses (batch)
all_details <- detail(results)

# Without progress updates
all_details <- detail(results, progress = FALSE)
} # }
```
