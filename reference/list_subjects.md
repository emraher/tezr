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

## Examples

``` r
if (FALSE) { # \dontrun{
subjects <- list_subjects()
} # }
```
