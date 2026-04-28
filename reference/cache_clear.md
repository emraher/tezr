# Clear the cache

Removes cached search results, thesis details, and/or lookup data from
memory.

## Usage

``` r
cache_clear(what = c("all", "searches", "details", "lookups"))
```

## Arguments

- what:

  Character. What to clear: "all", "searches", "details", or "lookups".
  Default is "all".

## Value

Invisible NULL

## Examples

``` r
# Clear everything
cache_clear()
#> ✔ Search cache cleared
#> ✔ Detail cache cleared
#> ✔ Lookup cache cleared

# Clear only search results
cache_clear("searches")
#> ✔ Search cache cleared

# Clear only thesis details
cache_clear("details")
#> ✔ Detail cache cleared
```
