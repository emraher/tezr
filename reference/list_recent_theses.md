# List recent theses from YOK Tez

Retrieves one of the recent-thesis lists exposed by YOK's TezIslemleri
endpoint. This is the only redesigned endpoint that lists theses without
a keyword.

## Usage

``` r
list_recent_theses(
  mode = c("last_15_days", "current_year"),
  max_search_results = 2000,
  ignore_cache = FALSE
)
```

## Arguments

- mode:

  Character. One of `"last_15_days"` or `"current_year"`.

- max_search_results:

  Maximum rows to return from the server-visible batch. Default is 2000.

- ignore_cache:

  Logical. If `TRUE`, bypass cached recent-list results and fetch fresh
  data from the server.

## Value

A tibble containing thesis records with the same columns as
[`search_basic()`](https://eremrah.com/tezr/reference/search_basic.md).

## Examples

``` r
if (FALSE) { # \dontrun{
recent <- list_recent_theses()
this_year <- list_recent_theses(mode = "current_year")
} # }
```
