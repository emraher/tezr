# Retrieve Thesis Metadata from Turkiye's National Thesis Center

`tezr` retrieves, parses, caches, and checks thesis metadata from
Turkiye's National Thesis Center (Ulusal Tez Merkezi), the Council of
Higher Education's public web portal for graduate thesis records.

## Responsible use

Use the package for academic, reproducible research workflows. Cache or
save large results locally, avoid repeated identical requests, and
respect the NTC portal's access controls and terms. Do not use the
package to bypass access restrictions or to scrape thesis full text at
scale.

## Request behavior

Requests use a package-identifying user agent by default. Override it
with `request_config(user_agent = "...")` or the `TEZR_USER_AGENT`
environment variable if needed. Set `request_config(verbose = FALSE)` or
`TEZR_VERBOSE=false` to silence informational progress messages.
Warnings and errors are still shown.

The package applies a two-second request delay, retries requests up to
three times, refreshes sessions after 50 logical requests or 20 minutes,
and caches searches, range chunks, details, and lookups in memory.
Search and detail parsing support both the legacy WATable response and
the redesigned card-based response observed on the portal in June 2026.

## Citation

Use `citation("tezr")` for the preferred package citation. Also cite the
National Thesis Center or Council of Higher Education as the source of
thesis metadata and include the retrieval date.

## See also

Useful links:

- <https://eremrah.com/tezr/>

- <https://github.com/emraher/tezr>

- Report bugs at <https://github.com/emraher/tezr/issues>

## Author

**Maintainer**: Emrah Er <eer@eremrah.com>
