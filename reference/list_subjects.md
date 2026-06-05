# List all available subjects

Returns all subjects in the National Thesis Center database. Turkish:
Konu

## Usage

``` r
list_subjects()
```

## Value

A tibble with three columns:

- name_tr - Character. Turkish subject name

- name_en - Character. English subject name

- id - Character. Internal API identifier

## See also

Other lookup functions:
[`list_disciplines()`](https://eremrah.com/tezr/reference/list_disciplines.md),
[`list_divisions()`](https://eremrah.com/tezr/reference/list_divisions.md),
[`list_institutes()`](https://eremrah.com/tezr/reference/list_institutes.md),
[`list_universities()`](https://eremrah.com/tezr/reference/list_universities.md)

## Examples

``` r
if (FALSE) { # interactive()
subjects <- list_subjects()
}
```
