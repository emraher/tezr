# Contributing to tezr

Thank you for considering a contribution to `tezr`.

This project follows the [rOpenSci Code of
Conduct](https://ropensci.org/code-of-conduct/). Please keep issues and
pull requests professional, reproducible, and free of sensitive data.

## Development Setup

Fork and clone the repository.

``` sh
git clone https://github.com/your-username/tezr.git
cd tezr
git checkout -b feature/your-feature-name
```

Install development dependencies from R.

``` r
install.packages("devtools")
devtools::install_dev_deps()
```

## Development Workflow

Write code in the existing package style. Add or update roxygen
documentation for exported functions. Add focused tests for new
behavior.

Run the core local checks before opening a pull request.

``` r
devtools::document()
devtools::test()
devtools::check()
```

Use `air format .` when Air is available. The repository also has an Air
formatting check in GitHub Actions.

## Tests

The default test suite uses fixtures and mocks. It should not contact
the National Thesis Center portal.

Run the default suite with:

``` r
devtools::test()
```

Live integration tests are skipped unless you explicitly enable them.

``` sh
TEZR_LIVE_TESTS=true Rscript -e 'devtools::test()'
```

Run live tests sparingly. They depend on portal availability, network
behavior, and current NTC markup. They can also take longer because the
package applies request delays.

## Documentation Examples

README and vignette builds do not run live NTC queries by default. Set
`TEZR_LIVE_EXAMPLES=true` only when you intentionally want documentation
chunks to contact the portal.

``` sh
TEZR_LIVE_EXAMPLES=true Rscript -e 'devtools::build_readme()'
TEZR_LIVE_EXAMPLES=true Rscript -e 'pkgdown::build_site()'
```

Save returned objects with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) or
[`readr::write_rds()`](https://readr.tidyverse.org/reference/read_rds.html)
when developing examples from live data. Avoid repeated identical portal
requests.

## Request Behavior

`tezr` identifies itself with a package-specific user agent. Use
`request_config(user_agent = "...")` if your institution or the portal
requires a different value.

Use `request_config(verbose = FALSE)` or `TEZR_VERBOSE=false` to silence
informational progress messages during local development. Warnings and
errors are still shown.

## Issue Reports

A useful bug report includes the function call, a small reproducible
example, the returned error or warning, your package version, your R
version, and whether `TEZR_LIVE_TESTS` or `TEZR_LIVE_EXAMPLES` was
enabled.

Do not post private researcher data, thesis full-text files, local
cookies, session headers, access tokens, or sensitive institutional
details. Redact query terms if they reveal private research plans.

## Pull Requests

Before submitting a pull request, make sure these checks pass locally
when feasible.

``` r
devtools::document()
devtools::test()
devtools::check()
urlchecker::url_check()
```

Also update `NEWS.md` for user-facing changes.
