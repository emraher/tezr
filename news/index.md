# Changelog

## tezr 0.2.0

This release restores compatibility with the redesigned YOK thesis
search pages.

### Search

- [`search_basic()`](https://eremrah.com/tezr/reference/search_basic.md),
  [`search_advanced()`](https://eremrah.com/tezr/reference/search_advanced.md),
  and
  [`search_detailed()`](https://eremrah.com/tezr/reference/search_detailed.md)
  now work with the redesigned YOK search results.
- Large search results parse much faster, including searches that return
  the full 2000-result server batch.
- Switching between keyword and detailed searches in one R session no
  longer returns stale results from the previous search.
- University filters in
  [`search_detailed()`](https://eremrah.com/tezr/reference/search_detailed.md)
  are more reliable for year- and thesis-type-filtered searches.
- [`list_recent_theses()`](https://eremrah.com/tezr/reference/list_recent_theses.md)
  lists theses added in the last 15 days or during the current
  publication year.

### Detail retrieval

- [`detail()`](https://eremrah.com/tezr/reference/detail.md) now accepts
  search-result rows directly. Use `detail(results[1, ])` for one thesis
  or `detail(results)` for batch retrieval.
- [`detail()`](https://eremrah.com/tezr/reference/detail.md) returns
  APA, IEEE, MLA, Chicago, and Harvard citation fields when YOK provides
  them.
- [`detail()`](https://eremrah.com/tezr/reference/detail.md) retries
  batch requests sequentially when the live site rejects a parallel
  detail request.

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
  paginate beyond the first server batch when year ranges can be split
  below the cap.

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
