# Get detailed information about a thesis

Retrieves the full details of one or more theses from the Turkish
National Thesis Center using encoded IDs from search results.

## Usage

``` r
detail(detail_id, progress = TRUE, ...)
```

## Arguments

- detail_id:

  Character vector. Encoded thesis detail identifier(s) from the
  `detail_id` column of search results. Treat these values as opaque.
  They may contain one or more portal identifiers depending on the
  current National Thesis Center markup.

- progress:

  Logical. Show text progress updates when fetching multiple theses?
  Default is TRUE.

- ...:

  Reserved for internal use.

## Value

A tibble with thesis details (one row per thesis). Columns (in order):

- thesis_no - Thesis number

- title_original - Original title

- title_translation - Title translation when available

- author - Author name

- advisor - Advisor name and title

- co_advisor - Co-advisor name(s). Multiple names are
  semicolon-separated, and absent values are `NA`.

- university - University name

- institute - Institute name

- division - Division name

- year - Year

- pages - Number of pages

- thesis_type_tr - Thesis type in Turkish, such as "Doktora" or "Yüksek
  Lisans"

- thesis_type_en - Thesis type in English, such as "Doctorate" or
  "Master's"

- language_tr - Language in Turkish, such as "Türkçe" or "İngilizce"

- language_en - Language in English (e.g., "Turkish", "English")

- subject_tr - Turkish subject classifications, semicolon-separated when
  multiple

- subject_en - English subject classifications, semicolon-separated when
  multiple

- abstract_original - Abstract in the thesis's original language when
  available

- abstract_translation - Translated abstract when available

- keywords_tr - Turkish keywords (includes keywords from index field)

- keywords_en - English keywords (includes keywords from index field)

- access_status - Access status (open/restricted)

- pdf_url - URL to download PDF (if available)

- detail_url - URL to the thesis detail page

## Examples

``` r
if (FALSE) { # interactive()
# Search for theses
results <- search_basic("panel veri")

# Get details for a single thesis
thesis_details <- detail(results$detail_id[1])

# Get details for multiple theses (batch)
all_details <- detail(detail_id = results$detail_id)

# Without progress updates
all_details <- detail(detail_id = results$detail_id, progress = FALSE)
}
```
