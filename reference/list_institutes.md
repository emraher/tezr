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

## Examples

``` r
if (FALSE) { # \dontrun{
insts <- list_institutes()
} # }
```
