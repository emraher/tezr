# Changelog

## tezr 0.1.0.9000

### rOpenSci review preparation

- Added
  [`request_config()`](https://eremrah.com/tezr/reference/request_config.md)
  for user-agent overrides and package-level verbosity control.
- Switched the default user agent from a browser-like header to a
  package-identifying header.
- Guarded README and vignette live examples behind
  `TEZR_LIVE_EXAMPLES=true`.
- Expanded README, vignette, and contribution guidance for responsible
  use, source limitations, citation, maintenance, and live-test
  behavior.
- Added rOpenSci `pkgcheck` GitHub Actions coverage and repository Code
  of Conduct guidance.
- [`search_basic()`](https://eremrah.com/tezr/reference/search_basic.md),
  [`search_advanced()`](https://eremrah.com/tezr/reference/search_advanced.md),
  [`search_detailed()`](https://eremrah.com/tezr/reference/search_detailed.md),
  and [`detail()`](https://eremrah.com/tezr/reference/detail.md) now
  handle the redesigned National Thesis Center result-card markup and
  paired encoded detail identifiers.

## tezr 0.1.0

Initial release.

### Search

- [`search_basic()`](https://eremrah.com/tezr/reference/search_basic.md)
  performs keyword search across all search fields with type and access
  type filters.
- [`search_advanced()`](https://eremrah.com/tezr/reference/search_advanced.md)
  adds year range, language, group, university/institute, and status
  filters to keyword search.
- [`search_detailed()`](https://eremrah.com/tezr/reference/search_detailed.md)
  provides field-specific and institutional filters (university,
  division, subject, discipline, supervisor). Accepts vector-valued
  parameters with automatic expansion and deduplication.
- Automatic pagination via adaptive year-range splitting when results
  exceed the 2000-result server limit. Set `max_search_results = Inf` to
  retrieve all matching records.

### Detail retrieval

- [`detail()`](https://eremrah.com/tezr/reference/detail.md) fetches
  full thesis metadata (bilingual titles, abstracts, keywords,
  advisor/co-advisor, page count, PDF URL) for one or more theses.
  Supports batch retrieval with parallel requests.

### Lookup functions

- [`list_universities()`](https://eremrah.com/tezr/reference/list_universities.md),
  [`list_institutes()`](https://eremrah.com/tezr/reference/list_institutes.md),
  [`list_divisions()`](https://eremrah.com/tezr/reference/list_divisions.md),
  [`list_disciplines()`](https://eremrah.com/tezr/reference/list_disciplines.md),
  and
  [`list_subjects()`](https://eremrah.com/tezr/reference/list_subjects.md)
  return entity lists from the database with name-to-ID mappings.

### Statistics

- [`stats_years()`](https://eremrah.com/tezr/reference/stats_years.md),
  [`stats_universities()`](https://eremrah.com/tezr/reference/stats_universities.md),
  [`stats_subjects()`](https://eremrah.com/tezr/reference/stats_subjects.md),
  and
  [`stats_types()`](https://eremrah.com/tezr/reference/stats_types.md)
  return aggregate thesis counts directly from the portal.

### Cache management

- [`cache_config()`](https://eremrah.com/tezr/reference/cache_config.md)
  controls in-memory caching with configurable TTL.
- [`cache_info()`](https://eremrah.com/tezr/reference/cache_info.md)
  reports cache state and item counts.
- [`cache_clear()`](https://eremrah.com/tezr/reference/cache_clear.md)
  removes cached searches, details, and/or lookups.

### Infrastructure

- Session management with automatic cookie handling and refresh.
- Rate limiting (2-second delay between requests).
- SSL verification disabled by default due to server certificate issues.
