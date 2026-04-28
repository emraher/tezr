# Getting Started with \`tezr\`

This vignette walks through the core features of `tezr`. Each section
builds on the previous one, starting with simple keyword searches and
progressing to multi-filter queries, detail retrieval, and cache
management.

``` r
library(tezr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

`tezr` has three search functions:
[`search_basic()`](https://eremrah.com/tezr/reference/search_basic.md),
[`search_advanced()`](https://eremrah.com/tezr/reference/search_advanced.md),
and
[`search_detailed()`](https://eremrah.com/tezr/reference/search_detailed.md).
The sections below cover each function in order.

## Basic Search

[`search_basic()`](https://eremrah.com/tezr/reference/search_basic.md)
searches the [National Thesis Center (NTC)](https://tez.yok.gov.tr)
database by keyword. It checks all fields by default, so it works well
when you do not know where your term appears.

``` r
# Search all fields for "tarımsal sulama"
ag_irrigation <- search_basic("tarımsal sulama")
#> ℹ Initializing session...
#> ℹ Searching for: tarımsal sulama
#> ✔ Found 384 results
#> ✔ Returning 384 results
```

The output is a tibble with one row per thesis.

``` r
# Column names and types
dplyr::glimpse(ag_irrigation)
#> Rows: 384
#> Columns: 13
#> $ thesis_no         <chr> "678523", "919922", "726765", "537790", "47…
#> $ title_original    <chr> "Edirne ilinde tarımsal sulamada güneş ener…
#> $ title_translation <chr> "Solar energy use in agricultural irrigatio…
#> $ author            <chr> "UMUT KUZUCU", "RACİ NAR", "HALİS GAZİ HIZ"…
#> $ university        <chr> "TRAKYA ÜNİVERSİTESİ", "BATMAN ÜNİVERSİTESİ…
#> $ year              <int> 2021, 2025, 2022, 2018, 2017, 2016, 2005, 2…
#> $ thesis_type_tr    <chr> "Yüksek Lisans", "Yüksek Lisans", "Yüksek L…
#> $ thesis_type_en    <chr> "Master", "Master", "Master", "Master", "Ma…
#> $ language_tr       <chr> "Türkçe", "Türkçe", "Türkçe", "Türkçe", "Tü…
#> $ language_en       <chr> "Turkish", "Turkish", "Turkish", "Turkish",…
#> $ subject_tr        <chr> "Enerji", "Elektrik ve Elektronik Mühendisl…
#> $ subject_en        <chr> "Energy", "Electrical and Electronics Engin…
#> $ detail_id         <chr> "nktAch3WU1vxJbennA4Q0Q", "gQPyaJRMGYgVAtEp…
```

### Targeting Specific Fields

You can use the `search_field` argument to restrict matching to a single
field.

``` r
# Search only in thesis titles
ag_irrigation_title <- search_basic(
  "tarımsal sulama", 
  search_field = "title")
#> ℹ Searching for: tarımsal sulama
#> ✔ Found 57 results
#> ✔ Returning 57 results
dplyr::glimpse(ag_irrigation_title)
#> Rows: 57
#> Columns: 13
#> $ thesis_no         <chr> "991710", "866871", "728942", "624355", "62…
#> $ title_original    <chr> "Sustainability of wastewater recycling and…
#> $ title_translation <chr> "Tarımsal sulamada atıksuyun geri dönüşümü …
#> $ author            <chr> "BORA OKAN", "İRFAN ÖKTEM", "YAKUBU ABDULLA…
#> $ university        <chr> "İZMİR YÜKSEK TEKNOLOJİ ENSTİTÜSÜ", "ADANA …
#> $ year              <int> 2026, 2024, 2022, 2020, 2020, 2018, 2025, 2…
#> $ thesis_type_tr    <chr> "Doktora", "Yüksek Lisans", "Doktora", "Yük…
#> $ thesis_type_en    <chr> "Doctorate", "Master", "Doctorate", "Master…
#> $ language_tr       <chr> "İngilizce", "İngilizce", "İngilizce", "İng…
#> $ language_en       <chr> "English", "English", "English", "English",…
#> $ subject_tr        <chr> "Ziraat; Çevre Mühendisliği", "Elektrik ve …
#> $ subject_en        <chr> "Agriculture; Environmental Engineering", "…
#> $ detail_id         <chr> "3ZJCyPlL-NMTf4lsy4ai-w", "9c0Tkkgm7aOsztyU…
```

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
#> ℹ Searching for: ekonometri
#> ✔ Found 2005 results
#> ! Server limit: returning 2000 of 2005 results. Set `max_search_results = Inf` to auto-paginate.
dplyr::glimpse(phd_results)
#> Rows: 2,000
#> Columns: 13
#> $ thesis_no         <chr> "741760", "441334", "291611", "847927", "58…
#> $ title_original    <chr> "Hisse senedi fiyat tahmininde ekonometrik …
#> $ title_translation <chr> "Comparison of econometric and machine lear…
#> $ author            <chr> "KORAY YAPA", "FATMA İDİL BAKTEMUR", "FATMA…
#> $ university        <chr> "UŞAK ÜNİVERSİTESİ", "ÇUKUROVA ÜNİVERSİTESİ…
#> $ year              <int> 2022, 2016, 2011, 2024, 2019, 2014, 2008, 1…
#> $ thesis_type_tr    <chr> "Doktora", "Doktora", "Doktora", "Doktora",…
#> $ thesis_type_en    <chr> "Doctorate", "Doctorate", "Doctorate", "Doc…
#> $ language_tr       <chr> "Türkçe", "Türkçe", "Türkçe", "Türkçe", "İn…
#> $ language_en       <chr> "Turkish", "Turkish", "Turkish", "Turkish",…
#> $ subject_tr        <chr> "Ekonometri; İşletme", "Ekonometri", "Ekono…
#> $ subject_en        <chr> "Econometrics; Business Administration", "E…
#> $ detail_id         <chr> "mrKjRVZdXarSz-8InkP-Yg", "Y4MY10XzD7cdMim7…
```

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
#> ℹ Performing advanced search...
#> ✔ Found 4596 results
#> ℹ Auto-pagination: target 4596 results across 2018-2020
#> ℹ Range 2018-2018: 1218 results retrieved.
#> ℹ Range 2019-2019: 2000 results retrieved.
#> ! Range 2019-2019 has 2298 results but only 2000 returned (server limit). This is a single-year range and cannot split further. Some results were truncated. Add more filters to narrow the search.
#> ℹ Range 2020-2020: 1080 results retrieved.
#> ✔ Pagination complete: 4298 unique results
#> ! Returning 4298 of 4596 results.At least one single-year range exceeded the server limit and cannot split further (years: 2019). Add more filters (for example year, thesis type, university, subject).
dplyr::glimpse(all_eu)
#> Rows: 4,298
#> Columns: 13
#> $ thesis_no         <chr> "505128", "513160", "526658", "516229", "53…
#> $ title_original    <chr> "Kalkınma parametresi çerçevesinde Türkiye'…
#> $ title_translation <chr> "The place of Turkey in the future of Europ…
#> $ author            <chr> "YAKUP KARAKUŞ", "CANER ÖVSAN ÇAKAŞ", "TUĞÇ…
#> $ university        <chr> "YÜZÜNCÜ YIL ÜNİVERSİTESİ", "DOKUZ EYLÜL ÜN…
#> $ year              <int> 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2…
#> $ thesis_type_tr    <chr> "Yüksek Lisans", "Doktora", "Yüksek Lisans"…
#> $ thesis_type_en    <chr> "Master", "Doctorate", "Master", "Master", …
#> $ language_tr       <chr> "Türkçe", "Türkçe", "İngilizce", "Türkçe", …
#> $ language_en       <chr> "Turkish", "Turkish", "English", "Turkish",…
#> $ subject_tr        <chr> "Ekonomi; Uluslararası İlişkiler", "Din; Ul…
#> $ subject_en        <chr> "Economics; International Relations", "Reli…
#> $ detail_id         <chr> "UaiCHH-mBoLP20zqTCxNIw", "fB_iHRv8HpCJaTpM…
```

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
#> # A tibble: 6 × 2
#>   name                                                   id   
#>   <chr>                                                  <chr>
#> 1 ABANT İZZET BAYSAL ÜNİVERSİTESİ                        30   
#> 2 ABDULLAH GÜL ÜNİVERSİTESİ                              168  
#> 3 ACIBADEM MEHMET ALİ AYDINLAR ÜNİVERSİTESİ              196  
#> 4 ACIBADEM ÜNİVERSİTESİ                                  124  
#> 5 ADALET BAKANLIĞI                                       62   
#> 6 ADANA ALPARSLAN TÜRKEŞ BİLİM VE TEKNOLOJİ ÜNİVERSİTESİ 246
```

``` r
# Subjects have Turkish and English names
subjects <- list_subjects()
subjects |>
  filter(stringr::str_detect(name_tr, "Ekonomi"))
#> # A tibble: 3 × 3
#>   name_tr                                  name_en                id   
#>   <chr>                                    <chr>                  <chr>
#> 1 Çalışma Ekonomisi ve Endüstri İlişkileri Labour Economics and … 35   
#> 2 Ekonomi                                  Economics              56   
#> 3 Ev Ekonomisi                             Home Economics         64
```

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
#> ℹ Performing detailed search...
#> ✔ Found 8 results
#> ✔ Returning 8 results
head(supervisor_theses)
#> # A tibble: 6 × 13
#>   thesis_no title_original    title_translation author university  year
#>   <chr>     <chr>             <chr>             <chr>  <chr>      <int>
#> 1 889302    Özel okulların, … Evaluating the p… İLKE … ANKARA ÜN…  2024
#> 2 946178    İlave çalışan et… Added worker eff… DENİZ… ANKARA ÜN…  2022
#> 3 707357    Yolsuzluğun elek… The impact of co… FUNDA… ANKARA ÜN…  2021
#> 4 621270    Elektronik ticar… Development of e… CANER… ANKARA ÜN…  2020
#> 5 631795    Ekonomide ağ mod… Network models a… ÖMER … ANKARA ÜN…  2020
#> 6 490687    İran'da bölgesel… The political ec… NASER… ANKARA ÜN…  2017
#> # ℹ 7 more variables: thesis_type_tr <chr>, thesis_type_en <chr>,
#> #   language_tr <chr>, language_en <chr>, subject_tr <chr>,
#> #   subject_en <chr>, detail_id <chr>
```

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
#> Warning: University "Ankara Üniversitesi" not found. Search may not filter by
#> university correctly.
#> Warning: Division "İktisat Ana Bilim Dalı" not found. Search may not filter by
#> division correctly.
#> ℹ Performing detailed search...
#> ✔ Found 27771 results
#> ! Returning 2000 of 27771 results. Set `max_search_results = Inf` to auto-paginate and retrieve all results.

ankara_econ_details <- detail(ankara_econ$detail_id[2])
#> ℹ Fetching thesis details...
#> ✔ Retrieved details for thesis

dplyr::glimpse(ankara_econ_details)
#> Rows: 1
#> Columns: 24
#> $ thesis_no            <chr> "913058"
#> $ title_original       <chr> "Türkiye'nin sosyo-politik dönüşümü ve S…
#> $ title_translation    <chr> "Turkey's socio-political transformation…
#> $ author               <chr> "MEHMET ŞİRİN BAKIR"
#> $ advisor              <chr> "DOÇ. DR. IŞIL ARPACI"
#> $ co_advisor           <chr> NA
#> $ university           <chr> "İNÖNÜ ÜNİVERSİTESİ"
#> $ institute            <chr> "SOSYAL BİLİMLER ENSTİTÜSÜ"
#> $ division             <chr> "SİYASET BİLİMİ VE KAMU YÖNETİMİ ANABİLİ…
#> $ year                 <chr> "2025"
#> $ pages                <chr> "185"
#> $ thesis_type_tr       <chr> "Doktora"
#> $ thesis_type_en       <chr> "Doctorate"
#> $ language_tr          <chr> "Türkçe"
#> $ language_en          <chr> "Turkish"
#> $ subject_tr           <chr> "Siyasal Bilimler"
#> $ subject_en           <chr> "Political Science"
#> $ abstract_original    <chr> "Mutlakiyetçi krallıkların ortaya çıkmas…
#> $ abstract_translation <chr> "Minorities, which became a problem with…
#> $ keywords_tr          <chr> NA
#> $ keywords_en          <chr> NA
#> $ access_status        <chr> "open"
#> $ pdf_url              <chr> "https://tez.yok.gov.tr/UlusalTezMerkezi…
#> $ detail_url           <chr> "https://tez.yok.gov.tr/UlusalTezMerkezi…

# English abstract
ankara_econ_details$abstract_translation
#> [1] "Minorities, which became a problem with the emergence of absolutist kingdoms, caused sectarian wars in Europe during the Reformation. In this period, the Ottoman Empire, which carried out a different minority policy from its contemporaries, ensured that communities lived together in tolerance thanks to the 'Millet System'. The Republic of Turkey, which was established after the collapse of the Ottoman Empire, had a trust problem towards minorities on the grounds that they collaborated with the enemy in the wars that took place in the last years of the Ottoman Empire. This situation has been reflected in state-minority relations for most of the history of the Republic. The Lausanne Peace Treaty, which determined minorities and minority rights, also determined the minority administration of Turkey. However, despite the Treaty of Lausanne, there have been problems regarding minorities in practice, and one of these problems is the exclusion of Assyrians from the minority status. Despite the definition of non-Muslim citizens as minorities in Lausanne, Assyrians could not exercise their minority rights until 2003. In this study, how the Assyrians have been affected by the social and political policies implemented from the foundation of the Republic to the present day and the socio-political problems of the Assyrians are discussed. In this context, Assyrians' relations with the state and issues such as representation, participation in politics, social relations and the phenomenon of migration are discussed, and the data obtained through empirical studies are analysed comparatively."
```

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
#> ℹ Fetching statistics...
tail(year_stats)
#> # A tibble: 6 × 8
#>    year yuksek_lisans doktora tipta_uzmanlik sanatta_yeterlik
#>   <int>         <int>   <int>          <int>            <int>
#> 1  2021         36462    8772           4847              151
#> 2  2022         46263   11380           5914              225
#> 3  2023         45209   13033           6842              227
#> 4  2024         47005   13947           6592              246
#> 5  2025         43142   13544           6918              258
#> 6  2026          2697     998            311               30
#> # ℹ 3 more variables: dis_hekimligi_uzmanlik <int>,
#> #   tipta_yan_dal_uzmanlik <int>, toplam <int>
```

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
#> $enabled
#> [1] TRUE
#> 
#> $search_count
#> [1] 6
#> 
#> $range_count
#> [1] 3
#> 
#> $detail_count
#> [1] 1
#> 
#> $search_ttl
#> [1] 3600
#> 
#> $detail_ttl
#> NULL
```

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
  can save results to disk with `readr::write_rds()` or
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) for persistence
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
  with `readr::write_rds()`, `readr::write_csv()`, or similar functions.
  Then reload from disk in later sessions.
- **Filter before paginating.** Add year ranges, thesis types, and
  institutional filters to keep result sets manageable before setting
  `max_search_results = Inf`.
- **Minimize server load.** Use cached results when possible. Avoid
  repeating identical queries.
- **Validate data quality.** Metadata may have inconsistencies (missing
  fields, encoding issues). Clean and validate before analysis.
