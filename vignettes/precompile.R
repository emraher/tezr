# Refresh documentation output without requiring CI to query tez.yok.gov.tr.
#
# Ordinary renders use representative data from vignettes/example-output.R.
# Set TEZR_LIVE_EXAMPLES=true before running this script only when you
# intentionally want to refresh output from the live portal.

required_files <- c(
  "README.Rmd",
  "vignettes/getting-started.Rmd",
  "vignettes/analysis-examples.Rmd",
  "vignettes/example-output.R"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  cli::cli_abort(c(
    "Documentation refresh cannot start.",
    "x" = "Missing required file{?s}: {missing_files}"
  ))
}

live_examples <- identical(tolower(Sys.getenv("TEZR_LIVE_EXAMPLES")), "true")
if (live_examples) {
  cli::cli_alert_warning(
    "Live documentation examples are enabled and will query tez.yok.gov.tr."
  )
} else {
  cli::cli_alert_info(
    "Using representative example output from vignettes/example-output.R."
  )
}

cli::cli_alert_info("Rendering README.md")
rmarkdown::render(
  "README.Rmd",
  output_format = "github_document",
  quiet = TRUE
)

cli::cli_alert_info("Rendering vignettes")
for (vignette in required_files[grepl(
  "^vignettes/.+[.]Rmd$",
  required_files
)]) {
  rmarkdown::render(
    vignette,
    output_format = "rmarkdown::html_vignette",
    quiet = TRUE
  )
}

cli::cli_alert_success("Documentation output refreshed")
cli::cli_alert_info("Run pkgdown::build_site() to inspect the website locally.")
