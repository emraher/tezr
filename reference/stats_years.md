# Get thesis statistics by year

Get thesis statistics by year

## Usage

``` r
stats_years()
```

## Value

A tibble containing thesis counts per year and type.

## See also

Other statistics functions:
[`stats_subjects()`](https://eremrah.com/tezr/reference/stats_subjects.md),
[`stats_types()`](https://eremrah.com/tezr/reference/stats_types.md),
[`stats_universities()`](https://eremrah.com/tezr/reference/stats_universities.md)

## Examples

``` r
if (FALSE) { # interactive()
stats <- stats_years()
tail(stats)
#> # A tibble: 6 x 4
#>    year yuksek_lisans doktora toplam
#>   <int>         <int>   <int>  <int>
#> 1  2025         22615    4416  27031
}
```
