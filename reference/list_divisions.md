# List all available divisions

Returns all divisions (Anabilim Dalı values) in the National Thesis
Center database. Turkish: Anabilim Dalı (ABD)

## Usage

``` r
list_divisions()
```

## Value

A tibble with two columns:

- name - Character. Division name

- id - Character. Internal API identifier

## See also

Other lookup functions:
[`list_disciplines()`](https://eremrah.com/tezr/reference/list_disciplines.md),
[`list_institutes()`](https://eremrah.com/tezr/reference/list_institutes.md),
[`list_subjects()`](https://eremrah.com/tezr/reference/list_subjects.md),
[`list_universities()`](https://eremrah.com/tezr/reference/list_universities.md)

## Examples

``` r
if (FALSE) { # interactive()
divisions <- list_divisions()
}
```
