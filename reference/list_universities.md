# List all available universities

Returns all universities in the National Thesis Center database.
Turkish: Üniversite

## Usage

``` r
list_universities()
```

## Value

A tibble with two columns:

- name - Character. University name

- id - Character. Internal API identifier

## See also

Other lookup functions:
[`list_disciplines()`](https://eremrah.com/tezr/reference/list_disciplines.md),
[`list_divisions()`](https://eremrah.com/tezr/reference/list_divisions.md),
[`list_institutes()`](https://eremrah.com/tezr/reference/list_institutes.md),
[`list_subjects()`](https://eremrah.com/tezr/reference/list_subjects.md)

## Examples

``` r
if (FALSE) { # interactive()
unis <- list_universities()
}
```
