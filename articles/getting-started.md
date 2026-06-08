# Getting Started with \`tezr\`

This vignette walks through the core features of `tezr`. Each section
builds on the previous one, starting with simple keyword searches and
progressing to multi-filter queries, detail retrieval, and cache
management.

The vignette does not run live NTC queries during ordinary package
builds. It uses representative output so readers can see the returned
shapes without depending on the live portal. Set
`TEZR_LIVE_EXAMPLES=true` before rendering if you want to execute the
portal requests.

``` r
library(tezr)
library(dplyr)
```

`tezr` has three search functions:
[`search_basic()`](https://eremrah.com/tezr/reference/search_basic.md),
[`search_advanced()`](https://eremrah.com/tezr/reference/search_advanced.md),
and
[`search_detailed()`](https://eremrah.com/tezr/reference/search_detailed.md).
The sections below cover each function in order.

## Responsible Use

Use `tezr` for academic, reproducible research workflows. Cache or save
large results locally, avoid repeated identical requests, and respect
the NTC portal’s access controls and terms. Do not post private
researcher data, access tokens, local cookies, or sensitive
institutional details in issues or pull requests.

When publishing results, cite both `tezr` and the National Thesis Center
or Council of Higher Education data source. Record the query terms,
filters, retrieval date, and any completeness warnings returned by the
package.

## More Than Downloading

`tezr` does more than submit a web form and return rows. It resolves
lookup labels, filters searches through structured portal fields,
paginates capped result sets by adaptive year ranges, parses bilingual
metadata, deduplicates expanded searches, annotates results with
completeness attributes, caches repeated requests, and exposes
statistics tables that help check retrieval completeness.

## Basic Search

[`search_basic()`](https://eremrah.com/tezr/reference/search_basic.md)
searches the [National Thesis Center (NTC)](https://tez.yok.gov.tr)
database by keyword. It checks all fields by default, so it works well
when you do not know where your term appears.

``` r
# Search all fields for "tarımsal sulama"
ag_irrigation <- search_basic("tarımsal sulama")
```

The output is a tibble with one row per thesis.

``` r
# Column names and types
dplyr::glimpse(ag_irrigation)
```

    #> Rows: 8
    #> Columns: 13
    #> $ thesis_no         <chr> "967755", "975988", "955779", "974976", "96…
    #> $ title_original    <chr> "Tarimsal sulama verimliligi 1", "Tarimsal …
    #> $ title_translation <chr> "Agricultural irrigation efficiency 1", "Ag…
    #> $ author            <chr> "PERIHAN EZGI BALLI", "CENAP ALAYBEYI", "SE…
    #> $ university        <chr> "Bandirma Onyedi Eylul Universitesi", "Harr…
    #> $ year              <int> 2025, 2025, 2025, 2024, 2023, 2022, 2021, 2…
    #> $ thesis_type_tr    <chr> "Doktora", "Yuksek Lisans", "Yuksek Lisans"…
    #> $ thesis_type_en    <chr> "Doctorate", "Master", "Master", "Master", …
    #> $ language_tr       <chr> "Turkce", "Turkce", "Turkce", "Turkce", "Tu…
    #> $ language_en       <chr> "Turkish", "Turkish", "Turkish", "Turkish",…
    #> $ subject_tr        <chr> "Ziraat; Ekonomi", "Ziraat; Ekonomi", "Zira…
    #> $ subject_en        <chr> "Agriculture; Economics", "Agriculture; Eco…
    #> $ detail_id         <chr> "TCKf4ksTOVsOBqUcPYMKWQ", "LypZzbdoWcG0f3c6…

### Targeting Specific Fields

You can use the `search_field` argument to restrict matching to a single
field.

``` r
# Search only in thesis titles
ag_irrigation_title <- search_basic(
  "tarımsal sulama",
  search_field = "title")
dplyr::glimpse(ag_irrigation_title)
```

    #> Rows: 3
    #> Columns: 13
    #> $ thesis_no         <chr> "967755", "975988", "955779"
    #> $ title_original    <chr> "Tarimsal sulama verimliligi 1", "Tarimsal …
    #> $ title_translation <chr> "Agricultural irrigation efficiency 1", "Ag…
    #> $ author            <chr> "PERIHAN EZGI BALLI", "CENAP ALAYBEYI", "SE…
    #> $ university        <chr> "Bandirma Onyedi Eylul Universitesi", "Harr…
    #> $ year              <int> 2025, 2025, 2025
    #> $ thesis_type_tr    <chr> "Doktora", "Yuksek Lisans", "Yuksek Lisans"
    #> $ thesis_type_en    <chr> "Doctorate", "Master", "Master"
    #> $ language_tr       <chr> "Turkce", "Turkce", "Turkce"
    #> $ language_en       <chr> "Turkish", "Turkish", "Turkish"
    #> $ subject_tr        <chr> "Ziraat; Ekonomi", "Ziraat; Ekonomi", "Zira…
    #> $ subject_en        <chr> "Agriculture; Economics", "Agriculture; Eco…
    #> $ detail_id         <chr> "TCKf4ksTOVsOBqUcPYMKWQ", "LypZzbdoWcG0f3c6…

Available search field values are: `"all"` (default), `"title"`,
`"author"`, `"supervisor"`, `"subject"`, `"index"`, `"abstract"`, and
`"thesis_no"`.

``` r
# Search abstracts
abstract_search <- search_basic(
  "production function",
  search_field = "abstract")

# Search by author name
author_search <- search_basic(
  "Işıl Şirin Selçuk",
  search_field = "author")

# Search by thesis number
number_search <- search_basic(
  "889301",
  search_field = "thesis_no")
```

### Filtering by Thesis Type and Access Status

[`search_basic()`](https://eremrah.com/tezr/reference/search_basic.md)
also accepts `thesis_type` and `access_type` filters. These filters are
applied server-side, so you download fewer records.

Available `thesis_type` values are: `"all"` (default), `"masters"`,
`"phd"`, `"medical_specialty"`, `"arts"`, `"dentistry"`,
`"medical_sub"`, `"pharmacy"`.

``` r
# PhD dissertations only
phd_results <- search_basic(
  "ekonometri",
  thesis_type = "phd")
dplyr::glimpse(phd_results)
```

    #> Rows: 4
    #> Columns: 13
    #> $ thesis_no         <chr> "967755", "960162", "946580", "968778"
    #> $ title_original    <chr> "Ekonometri uygulamalari 1", "Ekonometri uy…
    #> $ title_translation <chr> "Applications of econometrics 1", "Applicat…
    #> $ author            <chr> "PERIHAN EZGI BALLI", "SAMI KAYA", "ADIL MA…
    #> $ university        <chr> "Bandirma Onyedi Eylul Universitesi", "Ista…
    #> $ year              <int> 2025, 2023, 2022, 2020
    #> $ thesis_type_tr    <chr> "Doktora", "Doktora", "Doktora", "Doktora"
    #> $ thesis_type_en    <chr> "Doctorate", "Doctorate", "Doctorate", "Doc…
    #> $ language_tr       <chr> "Turkce", "Turkce", "Ingilizce", "Turkce"
    #> $ language_en       <chr> "Turkish", "Turkish", "English", "Turkish"
    #> $ subject_tr        <chr> "Ekonometri; Ekonomi", "Ekonometri; Ekonomi…
    #> $ subject_en        <chr> "Econometrics; Economics", "Econometrics; E…
    #> $ detail_id         <chr> "TCKf4ksTOVsOBqUcPYMKWQ", "h8m0hAyVIXV5l0pz…

Available access type values are: `"all"` (default), `"open"`,
`"restricted"`.

``` r
# Open access theses only
open_results <- search_basic(
  "hanehalkı",
  access_type = "open")
```

### The 2000-Result Limit

Basic search cannot exceed 2000 results. This is a server-side limit. If
your query returns more than 2000 records, the function warns you. In
these cases, you can set `max_search_results = Inf` to paginate past the
limit.
[`search_basic()`](https://eremrah.com/tezr/reference/search_basic.md)
automatically delegates to advanced search for pagination when you set
`max_search_results = Inf`. There is more information about pagination
below.

``` r
# This stops at 2000
climate_change <- search_basic("climate change")

# Delegate to advanced search with auto-pagination
climate_change_all <- search_basic(
  keyword = "climate change",
  max_search_results = Inf
)
```

## Advanced Search

[`search_advanced()`](https://eremrah.com/tezr/reference/search_advanced.md)
adds year range, language, group, university/institute, and thesis
status filters to keyword search.

The NTC advanced search form supports up to three keyword rows combined
with Boolean operators (`AND`, `OR`, `NOT`), each targeting a different
field.
[`search_advanced()`](https://eremrah.com/tezr/reference/search_advanced.md)
exposes only the first keyword row.

R packages that interface with academic databases, such as
[rentrez](https://docs.ropensci.org/rentrez/) (PubMed) and
[europepmc](https://docs.ropensci.org/europepmc/) (Europe PMC), often
pass Boolean logic as a single query string (for example,
`"term1 AND term2"`). NTC does not accept free-form Boolean strings. It
uses structured form fields for each keyword row, so that pattern is not
applicable here. To keep the interface simple,
[`search_advanced()`](https://eremrah.com/tezr/reference/search_advanced.md)
does not expose Boolean row combinations. For equivalent results, you
can use the following approaches.

- **AND**: Use
  [`search_detailed()`](https://eremrah.com/tezr/reference/search_detailed.md)
  with its field-specific parameters (`title`, `author`, `supervisor`,
  etc.).

- **OR**: Run separate searches and combine with
  `dplyr::bind_rows() |> dplyr::distinct()`.

- **NOT**: Run both searches and exclude with
  [`dplyr::anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html).

### Year and Language Filters

``` r
# Keyword search with year range
recent_climate <- search_advanced(
  keyword = "iklim değişikliği",
  year_start = 2015,
  year_end = 2024
)

# English-language theses only
# language accepts ISO 639 codes ("tr", "en", "fr", "de", ...), or
# full names ("Turkish", "French")
english_growth <- search_advanced(
  keyword = "economic growth",
  language = "en"
)

# French-language theses
french_theses <- search_advanced(
  keyword = "migration",
  language = "fr"
)
```

### Group and Thesis Status

`group` limits results to a broad group: `"all"` (default), `"science"`
(Fen Bilimleri), `"social"` (Sosyal Bilimler), `"medical"` (Tıp ve
Sağlık Bilimleri).

`status` controls whether results include only approved theses or also
in-preparation ones: `"approved"` (default), `"all"`,
`"in_preparation"`.

``` r
# Social sciences only
social_econ <- search_advanced(
  keyword = "ekonometri",
  group = "social"
)

# In-preparation theses (not yet defended)
ongoing_ml <- search_advanced(
  keyword = "makine öğrenmesi",
  status = "in_preparation"
)
```

### Combining Filters

You can combine filters to build precise keyword queries. Start with a
minimal query, then add constraints.

``` r
# PhD theses in social sciences, open access, 2000-2024
complex_query <- search_advanced(
  keyword = "ekonometri",
  search_field = "title",
  thesis_type = "phd",
  year_start = 2000,
  year_end = 2024,
  group = "social",
  access_type = "open"
)
```

### Auto-Pagination

When `max_search_results` is greater than 2000 (including `Inf`) and the
server reports more than 2000 matches, `tezr` switches to iterative
year-range pagination. If you do not supply `year_start` and `year_end`,
the package uses `1959:current_year` as the search window. It then
creates year chunks with weighted split points (pre-2000, 2000-2010,
post-2010) and a safety target below the hard 2000-row cap. Each chunk
is requested with the same filters as the original query. If a chunk is
still capped by the server limit, that chunk is split again and retried
until the range is small enough (or a single year remains). During this
process, `tezr` updates split weights from observed uncapped chunk
densities to bias later splits toward denser periods. Finally, chunk
results are merged, deduplicated by `thesis_no`, and returned. If a
single year still exceeds 2000 results, the package cannot paginate
further for that year and warns you to narrow the query with additional
filters.

``` r
# Retrieve all results (auto-paginate by year)
all_eu <- search_advanced(
  keyword = "avrupa",
  search_field = "all",
  year_start = 2018,
  year_end = 2020,
  max_search_results = Inf
)
dplyr::glimpse(all_eu)
```

    #> Rows: 8
    #> Columns: 13
    #> $ thesis_no         <chr> "967755", "975988", "955779", "974976", "96…
    #> $ title_original    <chr> "Avrupa Birligi ve Turkiye 1", "Avrupa Birl…
    #> $ title_translation <chr> "European Union and Turkiye 1", "European U…
    #> $ author            <chr> "PERIHAN EZGI BALLI", "CENAP ALAYBEYI", "SE…
    #> $ university        <chr> "Bandirma Onyedi Eylul Universitesi", "Harr…
    #> $ year              <int> 2025, 2025, 2025, 2024, 2023, 2022, 2021, 2…
    #> $ thesis_type_tr    <chr> "Doktora", "Yuksek Lisans", "Yuksek Lisans"…
    #> $ thesis_type_en    <chr> "Doctorate", "Master", "Master", "Master", …
    #> $ language_tr       <chr> "Turkce", "Turkce", "Turkce", "Turkce", "Tu…
    #> $ language_en       <chr> "Turkish", "Turkish", "Turkish", "Turkish",…
    #> $ subject_tr        <chr> "Avrupa Birligi; Ekonomi", "Avrupa Birligi;…
    #> $ subject_en        <chr> "European Union; Economics", "European Unio…
    #> $ detail_id         <chr> "TCKf4ksTOVsOBqUcPYMKWQ", "LypZzbdoWcG0f3c6…

## Detailed Search

[`search_detailed()`](https://eremrah.com/tezr/reference/search_detailed.md)
provides field-specific and institutional filters. Use it when you need
to target thesis titles, authors, supervisors, universities, divisions,
disciplines, or subjects. It supports the same auto-pagination flow as
[`search_advanced()`](https://eremrah.com/tezr/reference/search_advanced.md).

Available parameters in
[`search_detailed()`](https://eremrah.com/tezr/reference/search_detailed.md)
are: `thesis_no`, `title`, `author`, `supervisor`, `abstract`,
`keyword`, `university`, `university_id`, `institute`, `institute_id`,
`division`, `division_id`, `subject`, `discipline`, `discipline_id`,
`thesis_type`, `year_start`, `year_end`, `language`, `access_type`,
`group`, `status`, `max_search_results`, `ignore_cache`.

You should provide at least one search criterion or filter to use the
function.

### Finding Valid Filter Values

You can use the `list_*()` functions to discover valid filter names from
the server. In normal use, you can pass names (for example
`university = "Ankara Üniversitesi"`) into functions, and `tezr` will
handle ID mapping internally. The `id` columns are mainly lookup keys
used by `tezr` but you can also pass IDs directly for advanced or
performance-sensitive workflows (for example `university_id`,
`institute_id`, `division_id`, `discipline_id`) if you want.

``` r
# All universities
unis <- list_universities()
head(unis)
```

    #> # A tibble: 3 × 2
    #>   name                          id   
    #>   <chr>                         <chr>
    #> 1 ANKARA UNIVERSITESI           2    
    #> 2 ISTANBUL UNIVERSITESI         3    
    #> 3 ORTA DOGU TEKNIK UNIVERSITESI 60

``` r
# Subjects have Turkish and English names
subjects <- list_subjects()
subjects |>
  filter(stringr::str_detect(name_tr, stringr::fixed("Ekonomi")))
```

    #> # A tibble: 1 × 3
    #>   name_tr name_en   id   
    #>   <chr>   <chr>     <chr>
    #> 1 Ekonomi Economics 115

``` r
# Other list functions (each returns 'name' and 'id' columns)
institutes <- list_institutes()
divisions <- list_divisions()
disciplines <- list_disciplines()
```

### Filtering by Institution

You can pass university, institute, or division names as strings. `tezr`
resolves them to internal IDs automatically via lookup as we mentioned
above.

``` r
# All theses from Ankara University
ankara <- search_detailed(university = "Ankara Üniversitesi")
```

``` r
# Narrow to a specific division within a university
ankara_econ <- search_detailed(
  university = "Ankara Üniversitesi",
  division = "İktisat Ana Bilim Dalı"
)
```

``` r
# Filter by institute
sosyal_bilimler <- search_detailed(
  university = "İstanbul Üniversitesi",
  institute = "Sosyal Bilimler Enstitüsü"
)
```

### Filtering by Subject and Discipline

Subjects are broad categories (e.g., “Ekonometri”) whereas disciplines
are specializations (e.g., “İktisat Teorisi”). You can use
[`list_subjects()`](https://eremrah.com/tezr/reference/list_subjects.md)
and
[`list_disciplines()`](https://eremrah.com/tezr/reference/list_disciplines.md)
to confirm exact names.

``` r
# All econometrics theses
econ_all <- search_detailed(subject = "Ekonometri")
```

``` r
# Narrow to a discipline within a subject
theory <- search_detailed(
  subject = "Ekonomi",
  discipline = "İktisat Teorisi"
)
```

``` r
# Combine discipline with university
boun_theory <- search_detailed(
  university = "Boğaziçi Üniversitesi",
  discipline = "İktisat Teorisi"
)
```

### Filtering by Supervisor

You can also filter results by supervisor names.

``` r
# Find theses supervised by a specific supervisor
supervisor_theses <- search_detailed(supervisor = "Mustafa Kadir Doğan")
head(supervisor_theses)
```

    #> # A tibble: 6 × 13
    #>   thesis_no title_original    title_translation author university  year
    #>   <chr>     <chr>             <chr>             <chr>  <chr>      <int>
    #> 1 9677551   Parasal aktarım … Domestic credit … PERIH… Ankara Un…  2020
    #> 2 9759882   1980 sonrası Tür… Changes in sunfl… CENAP… Ankara Un…  2021
    #> 3 9557793   Doğrudan yabancı… Foreign direct i… SECIL… Ankara Un…  2022
    #> 4 9749764   Enerji kullanımı… Panel data analy… RUMEY… Ankara Un…  2023
    #> 5 9601625   Finansal liberal… Fragility during… SAMI … Ankara Un…  2024
    #> 6 9465806   Azerbaycan'ın ek… An analysis of A… ADIL … Ankara Un…  2025
    #> # ℹ 7 more variables: thesis_type_tr <chr>, thesis_type_en <chr>,
    #> #   language_tr <chr>, language_en <chr>, subject_tr <chr>,
    #> #   subject_en <chr>, detail_id <chr>

### Vector-Valued Parameters

The YÖK web portal accepts only one value per filter field. `tezr`
removes this restriction. Most
[`search_detailed()`](https://eremrah.com/tezr/reference/search_detailed.md)
parameters accept character vectors. When you pass multiple values, the
package expands them into separate API calls, combines the results, and
deduplicates by `thesis_no`. This makes cross-institutional and
cross-disciplinary comparisons possible in a single function call.

``` r
# Search across multiple universities
multi_uni <- search_detailed(
  university = c("Ankara Üniversitesi", "İstanbul Üniversitesi"),
  subject = "Ekonomi"
)

# Search multiple disciplines within a subject
multi_discipline <- search_detailed(
  subject = "Ekonomi",
  discipline = c("İktisat", "Maliye", "Ekonometri")
)

# Multiple thesis types
multi_type <- search_detailed(
  subject = "Ekonomi",
  thesis_type = c("phd", "masters")
)

# Multiple languages (ISO 639 codes)
multi_lang <- search_detailed(
  subject = "Ekonomi",
  language = c("tr", "en", "fr")
)

# Search across multiple universities with pagination
multi_uni_all <- search_detailed(
  university = c("Ankara Üniversitesi", "İstanbul Üniversitesi"),
  subject = "Ekonomi",
  max_search_results = Inf,
  ignore_cache = TRUE
)
```

## Retrieving Detailed Metadata

Search results contain core metadata (title, author, university, year,
type, subject). If you need full details, such as abstracts, keywords,
supervisor names, page counts, and PDF links, you can use
[`detail()`](https://eremrah.com/tezr/reference/detail.md) function.

### Single Thesis

You can pass a single `detail_id` from search results to
[`detail()`](https://eremrah.com/tezr/reference/detail.md). The function
returns a one-row tibble.

``` r
# Search and get details for the first match
ankara_econ <- search_detailed(
  university = "Ankara Üniversitesi",
  division = "İktisat Ana Bilim Dalı",
  thesis_type = "phd",
  year_start = 2024,
  year_end = 2025
)

ankara_econ_details <- detail(ankara_econ$detail_id[2])

dplyr::glimpse(ankara_econ_details)

# English abstract
ankara_econ_details$abstract_translation
```

    #> Rows: 1
    #> Columns: 24
    #> $ thesis_no            <chr> "9677551"
    #> $ title_original       <chr> "Parasal aktarim mekanizmasi cercevesind…
    #> $ title_translation    <chr> "Domestic credit to private sector and m…
    #> $ author               <chr> "PERIHAN EZGI BALLI"
    #> $ advisor              <chr> "PROF. DR. HASAN SAHIN"
    #> $ co_advisor           <chr> NA
    #> $ university           <chr> "Ankara Universitesi"
    #> $ institute            <chr> "Sosyal Bilimler Enstitusu"
    #> $ division             <chr> "Iktisat Ana Bilim Dali"
    #> $ year                 <chr> "2020"
    #> $ pages                <chr> "153"
    #> $ thesis_type_tr       <chr> "Doktora"
    #> $ thesis_type_en       <chr> "Doctorate"
    #> $ language_tr          <chr> "Turkce"
    #> $ language_en          <chr> "Turkish"
    #> $ subject_tr           <chr> "Ekonomi; Enerji"
    #> $ subject_en           <chr> "Economics; Energy"
    #> $ abstract_original    <chr> "Enerji piyasasi duzenlemelerinin ana ek…
    #> $ abstract_translation <chr> "This thesis consists of three essays on…
    #> $ keywords_tr          <chr> "Enerji piyasalari; Duzenleme; Elektrik"
    #> $ keywords_en          <chr> "Energy markets; Regulation; Electricity"
    #> $ access_status        <chr> "open"
    #> $ pdf_url              <chr> "https://tez.yok.gov.tr/UlusalTezMerkezi…
    #> $ detail_url           <chr> "https://tez.yok.gov.tr/UlusalTezMerkezi…
    #> [1] "This thesis consists of three essays on energy market regulation, market design, and transformation."

### Batch Retrieval

You can also pass a vector of `detail_id` values to fetch details for
multiple theses. The function shows text progress updates by default and
fetches uncached records in parallel (up to 5 active requests).

``` r
# Fetch details for all results
ankara_econ <- search_detailed(
  university = "Ankara Üniversitesi",
  division = "İktisat Ana Bilim Dalı",
  thesis_type = "phd",
  year_start = 2025,
  year_end = 2026
)

# Batch retrieval
ankara_econ_all_details <- detail(ankara_econ$detail_id)
```

## Aggregate Statistics

These functions return summary statistics tables from the NTC.

``` r
# Thesis counts by year
year_stats <- stats_years()
tail(year_stats)
```

    #> # A tibble: 5 × 4
    #>    year yuksek_lisans doktora toplam
    #>   <int>         <int>   <int>  <int>
    #> 1  2021         28906    7128  36034
    #> 2  2022         33102    7481  40583
    #> 3  2023         35987    8026  44013
    #> 4  2024         40124    8841  48965
    #> 5  2025         22615    4416  27031

``` r
# Thesis counts by university
uni_stats <- stats_universities()
head(uni_stats)

# Thesis counts by subject
subject_stats <- stats_subjects()
head(subject_stats)

# Total counts by thesis type
type_stats <- stats_types()
type_stats
```

## Cache Management

`tezr` caches search results, detail records, year-range queries, and
lookup lists in memory. Caching speeds up repeated queries and reduces
server load.

### Viewing Cache Status

``` r
# Shows: enabled status, item counts, and TTL settings
cache_info()
```

    #> $enabled
    #> [1] TRUE
    #> 
    #> $search_count
    #> [1] 0
    #> 
    #> $range_count
    #> [1] 0
    #> 
    #> $detail_count
    #> [1] 0
    #> 
    #> $search_ttl
    #> [1] 3600
    #> 
    #> $detail_ttl
    #> NULL

The output includes `search_count`, `range_count`, `detail_count`,
`search_ttl`, and `detail_ttl`. Search cache defaults to 3600 seconds (1
hour). Detail cache defaults to `NULL` (session lifetime; entries stay
until you clear them or restart R).

### Clearing Cache

You can clear specific cache types or everything at once. The
`"lookups"` option clears cached university/subject/division lists.

``` r
# Clear search results only
cache_clear("searches")

# Clear detail records only
cache_clear("details")

# Clear lookup lists (universities, subjects, etc.)
cache_clear("lookups")

# Clear everything
cache_clear("all")
```

### Configuring Cache TTL

You can also adjust time-to-live settings or disable caching entirely.
TTL values are in seconds. `NULL` means entries persist for the entire
session.

``` r
# 2-hour search cache, 1-week detail cache
cache_config(
  search_ttl = 7200,
  detail_ttl = 604800
)

# Disable caching entirely (every call hits the server)
cache_config(enable = FALSE)

# Re-enable with defaults
cache_config(enable = TRUE, search_ttl = 3600, detail_ttl = NULL)
```

## Request Configuration

`tezr` identifies itself with a package-specific user agent by default.
You can override that value for an institutional network policy or a
portal compatibility issue.

``` r
request_config(user_agent = "my-lab-contact@example.edu")
request_config(reset = TRUE)
```

Set `request_config(verbose = FALSE)` or `TEZR_VERBOSE=false` to silence
informational progress messages. Warnings and errors are still shown.

The package applies a two-second request delay, retries requests up to
three times, refreshes sessions after 50 logical requests or 20 minutes,
and uses in-memory caches for repeated searches, range chunks, details,
and lookups. Detail requests fetch uncached records in bounded parallel
batches.

## Working with Results

Search results are returned in tibbles, so they work directly with
`dplyr` and other `tidyverse` tools.

``` r
climate_change <- search_basic("climate change")

# Count by year
climate_change |>
  dplyr::count(year)

# Filter recent PhDs
climate_change |>
  dplyr::filter(thesis_type_tr == "Doktora", year >= 2020) |>
  dplyr::select(author, year, title_original, university)

# Most common subjects
climate_change |>
  dplyr::count(subject_tr, sort = TRUE) |>
  dplyr::slice_head(n = 10)
```

See the [Analysis
Examples](https://eremrah.com/tezr/articles/analysis-examples.md)
vignette for complete analysis workflows with visualizations.

## Limitations and Best Practices

### Technical Limitations

- **No official API.** `tezr` scrapes \<tez.yok.gov.tr\> by simulating
  browser requests and parsing HTML/JavaScript responses. Any change to
  the portal’s page structure, form parameters, or JavaScript patterns
  will break the package until updated. This is the primary fragility
  risk.
- **Single-year overflow.** Auto-pagination splits year ranges to work
  around the 2000-result server cap, but cannot split below a single
  calendar year. If a query matches more than 2000 theses in one year,
  the package retrieves only the first 2000 for that year and issues a
  warning. You should narrow your search with additional filters (thesis
  type, university, subject) to avoid this.
- **In-memory cache only.** All cached data (searches, details, lookups)
  is stored in R environment objects and lost when the session ends. You
  can save results to disk with
  [`readr::write_rds()`](https://readr.tidyverse.org/reference/read_rds.html)
  or [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) for persistence
  across sessions.
- **SSL verification disabled.** The YÖK server has certificate issues,
  so SSL peer verification is turned off (`ssl_verifypeer = FALSE`).
  This is a security trade-off required for the package to function.
- **Fixed rate limiting.** Requests use a built-in 2-second rate limit
  that is not user-configurable, so fetching large datasets still takes
  time.
- **Vector parameter expansion.**
  [`search_detailed()`](https://eremrah.com/tezr/reference/search_detailed.md)
  expands vector-valued parameters (e.g., multiple universities,
  subjects) into separate API calls via cartesian product. Passing many
  multi-valued parameters can generate a large number of requests (e.g.,
  10 universities × 5 subjects = 50 searches).
- **Lookup matching.** The `list_*()` lookup functions use exact and
  substring matching. Typos or minor name variations (for example,
  missing diacritics) will not match. Use
  [`list_universities()`](https://eremrah.com/tezr/reference/list_universities.md)
  and related lookup functions to confirm exact names before searching.
- **Metadata only.** The package retrieves thesis metadata. PDF URLs are
  included in detail records but full-text files are not downloaded. You
  can use URLs to download the PDFs.

### Best Practices

- **Cache and save results.** Run large queries once and save locally
  with
  [`readr::write_rds()`](https://readr.tidyverse.org/reference/read_rds.html),
  [`readr::write_csv()`](https://readr.tidyverse.org/reference/write_delim.html),
  or similar functions. Then reload from disk in later sessions.
- **Filter before paginating.** Add year ranges, thesis types, and
  institutional filters to keep result sets manageable before setting
  `max_search_results = Inf`.
- **Minimize server load.** Use cached results when possible. Avoid
  repeating identical queries.
- **Validate data quality.** Metadata may have inconsistencies (missing
  fields, encoding issues). Clean and validate before analysis.

## Citation

Use `citation("tezr")` for the preferred package citation. Also cite the
National Thesis Center or Council of Higher Education as the source of
thesis metadata and include the date you retrieved the data.
