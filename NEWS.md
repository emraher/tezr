# tezr 0.1.0

Initial release.

## Search

* `search_basic()` performs keyword search across all search fields with type
  and access type filters.
* `search_advanced()` adds year range, language, group,
  university/institute, and status filters to keyword search.
* `search_detailed()` provides field-specific and institutional filters
  (university, division, subject, discipline, supervisor). Accepts vector-valued
  parameters with automatic expansion and deduplication.
* Automatic pagination via adaptive year-range splitting when results exceed
  the 2000-result server limit. Set `max_search_results = Inf` to retrieve all
  matching records.

## Detail retrieval

* `detail()` fetches full thesis metadata (bilingual titles, abstracts,
  keywords, advisor/co-advisor, page count, PDF URL) for one or more theses.
  Supports batch retrieval with parallel requests.

## Lookup functions

* `list_universities()`, `list_institutes()`, `list_divisions()`,
  `list_disciplines()`, and `list_subjects()` return entity lists from the
  database with name-to-ID mappings.

## Statistics

* `stats_years()`, `stats_universities()`, `stats_subjects()`, and
  `stats_types()` return aggregate thesis counts directly from the portal.

## Cache management

* `cache_config()` controls in-memory caching with configurable TTL.
* `cache_info()` reports cache state and item counts.
* `cache_clear()` removes cached searches, details, and/or lookups.

## Infrastructure

* Session management with automatic cookie handling and refresh.
* Rate limiting (2-second delay between requests).
* SSL verification disabled by default due to server certificate issues.
