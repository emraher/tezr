# Get thesis statistics by university

Get thesis statistics by university

## Usage

``` r
stats_universities()
```

## Value

A tibble containing thesis counts per university and type.

## See also

Other statistics functions:
[`stats_subjects()`](https://eremrah.com/tezr/reference/stats_subjects.md),
[`stats_types()`](https://eremrah.com/tezr/reference/stats_types.md),
[`stats_years()`](https://eremrah.com/tezr/reference/stats_years.md)

## Examples

``` r
if (FALSE) { # interactive()
stats <- stats_universities()
head(stats)
#> # A tibble: 6 x 10
#>   university yuksek_lisans doktora toplam
#>   <chr>              <int>   <int>  <int>
#> 1 Ankara Univ...     32542   12310  44852
}
```
