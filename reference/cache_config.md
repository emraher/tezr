# Configure cache behavior

Controls whether search results and thesis details are cached in memory
during the R session. Caching can significantly speed up repeated
queries.

## Usage

``` r
cache_config(enable = TRUE, search_ttl = 3600, detail_ttl = NULL)
```

## Arguments

- enable:

  Logical. Enable caching? Default TRUE.

- search_ttl:

  Numeric. Search result TTL (time-to-live) in seconds. NULL means cache
  lives for entire session. Default is 3600 (1 hour).

- detail_ttl:

  Numeric. Detail page TTL in seconds. NULL means cache lives for entire
  session. Default is NULL (session).

## Value

Invisible NULL

## See also

Other cache functions:
[`cache_clear()`](https://eremrah.com/tezr/reference/cache_clear.md),
[`cache_info()`](https://eremrah.com/tezr/reference/cache_info.md)

## Examples

``` r
# Disable caching entirely
cache_config(enable = FALSE)

# Enable with 30-minute TTL for searches
cache_config(enable = TRUE, search_ttl = 1800)
info <- cache_info()
info[c("enabled", "search_ttl")]
#> $enabled
#> [1] TRUE
#> 
#> $search_ttl
#> [1] 1800
#> 
#> $enabled
#> [1] TRUE
#>
#> $search_ttl
#> [1] 1800

# Cache details forever (until session ends)
cache_config(detail_ttl = NULL)
```
