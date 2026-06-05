# List all available institutes

Returns all institutes in the National Thesis Center database. Turkish:
Enstitü

## Usage

``` r
list_institutes()
```

## Value

A tibble with two columns:

- name - Character. Institute name

- id - Character. Internal API identifier

## See also

Other lookup functions:
[`list_disciplines()`](https://eremrah.com/tezr/reference/list_disciplines.md),
[`list_divisions()`](https://eremrah.com/tezr/reference/list_divisions.md),
[`list_subjects()`](https://eremrah.com/tezr/reference/list_subjects.md),
[`list_universities()`](https://eremrah.com/tezr/reference/list_universities.md)

## Examples

``` r
if (FALSE) { # interactive()
insts <- list_institutes()
}
```
