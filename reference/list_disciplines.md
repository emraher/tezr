# List all available disciplines

Returns all disciplines (Bilim Dalı values) in the National Thesis
Center database. Turkish: Bilim Dalı

## Usage

``` r
list_disciplines()
```

## Value

A tibble with two columns:

- name - Character. Discipline name

- id - Character. Internal API identifier

## See also

Other lookup functions:
[`list_divisions()`](https://eremrah.com/tezr/reference/list_divisions.md),
[`list_institutes()`](https://eremrah.com/tezr/reference/list_institutes.md),
[`list_subjects()`](https://eremrah.com/tezr/reference/list_subjects.md),
[`list_universities()`](https://eremrah.com/tezr/reference/list_universities.md)

## Examples

``` r
if (FALSE) { # interactive()
disciplines <- list_disciplines()
head(disciplines)
#> # A tibble: 6 x 2
#>   name                         id
#>   <chr>                        <chr>
#> 1 ACIK DENIZ YAPILARI          1
#> 2 ADLI BILIMLER                2
}
```
