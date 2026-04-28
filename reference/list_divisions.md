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

## Examples

``` r
if (FALSE) { # \dontrun{
divisions <- list_divisions()
} # }
```
