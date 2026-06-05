# tezr

`tezr` retrieves, parses, caches, and checks thesis metadata from
Turkiye’s [National Thesis Center](https://tez.yok.gov.tr) (NTC, Ulusal
Tez Merkezi).

The NTC is the Council of Higher Education’s national portal for
graduate thesis records from Turkish universities. It exposes thesis
titles, authors, institutions, degree types, languages, subjects,
abstracts, advisors, page counts, access status, and detail-page links
through a public web interface. The portal contains close to one million
records, but it has no documented public API, no bulk export, and a
visible-result cap of 2,000 rows per query.

`tezr` turns that capped web-only archive into a scriptable R workflow.
It maps the basic, advanced, and detailed search forms into R functions.
It resolves valid university, institute, division, discipline, and
subject labels. It retrieves detail pages, parses bilingual metadata
into tibbles, deduplicates merged searches, caches repeated queries,
paginates large searches by adaptive year ranges, and exposes statistics
functions for retrieval checks.

`tezr` is not affiliated with, endorsed by, or connected to YOK, YÖK,
the Council of Higher Education, or the National Thesis Center.

## Installation

``` r

# install.packages("pak")
pak::pak("emraher/tezr")
```

## Quick Start

Live examples are not run when this README is built. Set
`TEZR_LIVE_EXAMPLES=true` before rendering the README or pkgdown site if
you want to run the portal queries.

``` r

library(tezr)
```

Search results use `title_original` and `title_translation`. Detail
records use `abstract_original` and `abstract_translation`.

``` r

household <- search_basic(keyword = "hanehalkı")
dplyr::glimpse(household)
```

``` r

climate_change <- search_advanced(
  keyword = "iklim değişikliği",
  year_start = 2015,
  group = "science"
)
```

``` r

phd_theses <- search_detailed(
  university = "Ankara Üniversitesi",
  division = "İktisat Ana Bilim Dalı",
  thesis_type = "phd",
  year_start = 2020
)
```

``` r

details <- detail(phd_theses$detail_id[1])

details |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "colname",
    values_to = "colvalue"
  ) |>
  print(n = 23)
```

Save returned objects after large queries so later analysis does not
repeat the same portal requests.

``` r

saveRDS(climate_change, "climate_change_theses.rds")
readr::write_rds(climate_change, "climate_change_theses.readr.rds")
```

## Scope

The primary rOpenSci category is data retrieval. The secondary use case
is bibliometrics and thesis metadata analysis.

Adjacent packages such as
[`rentrez`](https://docs.ropensci.org/rentrez/) and
[`europepmc`](https://docs.ropensci.org/europepmc/) wrap formal
scholarly APIs. `tezr` differs because the NTC only exposes a web portal
and uses structured form fields rather than a query language.
[`bibliometrix`](https://www.bibliometrix.org/) focuses on bibliometric
analysis after data are already available. `tezr` focuses on retrieval,
parsing, completeness checks, and preparation of NTC thesis metadata for
analysis. Turkish higher-education data packages, when available,
usually target institutional or public statistics sources rather than
the NTC thesis-record portal.

## Responsible Use

Use `tezr` for academic, reproducible research workflows. Respect the
NTC portal, avoid unnecessary repeated requests, and cache or save
results locally. Do not use the package to bypass access restrictions,
to scrape thesis full text at scale, or to redistribute metadata in ways
that conflict with the source portal’s terms.

When publishing results, cite both `tezr` and the NTC or Council of
Higher Education data source. Also document the query terms, filters,
retrieval dates, and any completeness warnings returned by `tezr`.

The project follows the [rOpenSci Code of
Conduct](https://ropensci.org/code-of-conduct/). Do not post private
researcher data, access tokens, local cookies, or sensitive
institutional details in issues or pull requests.

## Request Behavior

`tezr` sends a package-identifying user agent by default. Override it
with `request_config(user_agent = "...")` or `TEZR_USER_AGENT` if your
institution or the portal requires a different header.

``` r

request_config(user_agent = "my-lab-contact@example.edu")
request_config(reset = TRUE)
```

Set `request_config(verbose = FALSE)` or `TEZR_VERBOSE=false` to silence
informational progress messages. Warnings and errors are still shown.

The package applies a two-second request delay, retries requests up to
three times through `httr2`, refreshes sessions after 50 logical
requests or 20 minutes, and caches searches, range chunks, details, and
lookups in memory. Detail requests use bounded parallel fetching for
uncached records.

## More Than Downloading

`tezr` adds workflow behavior that is not available from the NTC web
interface.

- Filtered basic, advanced, and detailed search functions.
- Lookup resolution for valid NTC labels and IDs.
- Adaptive pagination for queries that exceed the 2,000-row portal cap.
- Parsing of bilingual search and detail metadata into rectangular
  tibbles.
- Deduplication for expanded multi-value detailed searches.
- Completeness attributes that record reported totals, pagination
  status, and single-year overflow.
- In-memory caching for repeated searches, detail pages, range chunks,
  and lookup lists.
- Aggregate statistics functions that help compare retrieved records
  against portal counts.

## Limitations

- The NTC has no public API. Markup, form fields, cookies, or JavaScript
  behavior may change without notice.
- Search results are capped at 2,000 rows per portal request.
- Auto-pagination cannot split below a single calendar year. Single-year
  overflow can still leave results incomplete.
- Metadata quality is uneven. Fields can be missing, duplicated,
  inconsistently translated, or encoded differently across records.
- The package retrieves metadata and detail-page links. It does not
  download thesis full text.
- The package depends on live portal availability. Network failures,
  certificate issues, and server-side blocking can affect retrieval.

## Maintenance

The package is maintained as active research software. The current
public API is intended to be review-ready, but the development version
may still make breaking changes in response to rOpenSci review before a
stable release. The maintainer commits to at least two years of
maintenance after review acceptance, including fixes for NTC markup
changes when feasible.

## Citation

Use `citation("tezr")` for the preferred package citation. Also cite the
National Thesis Center or Council of Higher Education as the source of
thesis metadata and include the date you retrieved the data.

## Learn More

- [Getting
  Started](https://eremrah.com/tezr/articles/getting-started.html)
- [Analysis
  Examples](https://eremrah.com/tezr/articles/analysis-examples.html)
- [Function Reference](https://eremrah.com/tezr/reference/index.html)
- [Contributing](https://eremrah.com/tezr/CONTRIBUTING.md)
