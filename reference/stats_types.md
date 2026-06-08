# Get thesis statistics by type

Get thesis statistics by type

## Usage

``` r
stats_types()
```

## Value

A tibble containing total thesis counts per type.

## See also

Other statistics functions:
[`stats_subjects()`](https://eremrah.com/tezr/reference/stats_subjects.md),
[`stats_universities()`](https://eremrah.com/tezr/reference/stats_universities.md),
[`stats_years()`](https://eremrah.com/tezr/reference/stats_years.md)

## Examples

``` r
if (FALSE) { # interactive()
stats <- stats_types()
stats
#> # A tibble: 1 x 4
#>   yuksek_lisans doktora tipta_uzmanlik sanatta_yeterlik
#>           <dbl>   <dbl>           <dbl>            <dbl>
#> 1        721384  178264           90030             2395
}
```
