# Get cache statistics

Returns information about the current cache state, including number of
cached items and configuration settings.

## Usage

``` r
cache_info()
```

## Value

A list with cache statistics:

- enabled - Whether caching is enabled

- search_count - Number of cached search results

- range_count - Number of cached year-range searches

- detail_count - Number of cached thesis details

- search_ttl - Search cache TTL in seconds (NULL = session)

- detail_ttl - Detail cache TTL in seconds (NULL = session)

## Examples

``` r
cache_info()
#> $enabled
#> [1] TRUE
#> 
#> $search_count
#> [1] 0
#> 
#> $range_count
#> [1] 0
#> 
#> $detail_count
#> [1] 0
#> 
#> $search_ttl
#> [1] 3600
#> 
#> $detail_ttl
#> NULL
#> 
```
