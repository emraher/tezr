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

## Examples

``` r
if (FALSE) { # \dontrun{
unis <- list_universities()
} # }
```
