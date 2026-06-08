# Configure request behavior

Sets package-level request options. `tezr` identifies itself with a
package-specific user agent by default. You can override that header
when required by a network policy or by the National Thesis Center
portal.

## Usage

``` r
request_config(user_agent = NULL, verbose = NULL, reset = FALSE)
```

## Arguments

- user_agent:

  Character. Optional user agent to send with HTTP requests.

- verbose:

  Logical. Show informational request messages?

- reset:

  Logical. Reset request options to package defaults before applying
  other arguments.

## Value

Invisible list with the active request configuration.

## Details

Informational messages are shown by default. Set `verbose = FALSE` to
silence progress and success messages. Warnings and errors are still
shown.

You can also set `TEZR_USER_AGENT` and `TEZR_VERBOSE` before starting R.
Explicit R options take precedence over environment variables.

## Examples

``` r
# Silence progress messages for the current R session
cfg <- request_config(verbose = FALSE)
cfg$verbose
#> [1] FALSE
#> [1] FALSE

# Set a custom user agent for an institutional network policy
cfg <- request_config(user_agent = "my-lab-contact@example.edu")
cfg$user_agent
#> [1] "my-lab-contact@example.edu"
#> [1] "my-lab-contact@example.edu"

# Return to package defaults
request_config(reset = TRUE)
```
