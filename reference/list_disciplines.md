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

## Examples

``` r
if (FALSE) { # \dontrun{
disciplines <- list_disciplines()
} # }
```
