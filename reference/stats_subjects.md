# Get thesis statistics by subject

Get thesis statistics by subject

## Usage

``` r
stats_subjects()
```

## Value

A tibble containing thesis counts per subject and type.

## See also

Other statistics functions:
[`stats_types()`](https://eremrah.com/tezr/reference/stats_types.md),
[`stats_universities()`](https://eremrah.com/tezr/reference/stats_universities.md),
[`stats_years()`](https://eremrah.com/tezr/reference/stats_years.md)

## Examples

``` r
if (FALSE) { # interactive()
stats <- stats_subjects()
head(stats)
#> # A tibble: 6 x 8
#>   subject   yuksek_lisans doktora toplam
#>   <chr>             <int>   <int>  <int>
#> 1 Ekonomi           21142    5842  26984
}
```
