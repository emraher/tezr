# Clear the cache

Removes cached search results, thesis details, and lookup data from
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

## See also

Other cache functions:
[`cache_config()`](https://eremrah.com/tezr/reference/cache_config.md),
[`cache_info()`](https://eremrah.com/tezr/reference/cache_info.md)

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
info <- cache_info()
info[c("search_count", "detail_count")]
#> $search_count
#> [1] 0
#> 
#> $detail_count
#> [1] 0
#> 
#> $search_count
#> [1] 0
#>
#> $detail_count
#> [1] 0
```
