# Precompile vignettes with web scraping
# Run this script to update vignettes with fresh data from tez.yok.gov.tr
#
# Based on: https://ropensci.org/blog/2019/12/08/precompute-vignettes/
#
# Usage: source("vignettes/precompile.R")

precompile_vignette <- function(vignette_name) {
  cli::cli_alert_info("Precompiling {vignette_name}")

  orig_path <- file.path("vignettes", paste0(vignette_name, ".Rmd.orig"))
  output_path <- file.path("vignettes", paste0(vignette_name, ".Rmd"))

  knitr::knit(orig_path, output = output_path)

  # Fix figure paths (knitr creates vignettes/figure/ but needs vignettes/)
  figure_dir <- "vignettes/figure"
  if (dir.exists(figure_dir)) {
    figure_files <- list.files(figure_dir, full.names = TRUE)
    file.copy(figure_files, "vignettes/", overwrite = TRUE)
    unlink(figure_dir, recursive = TRUE)

    vignette_content <- readr::read_file(output_path)
    fixed_content <- stringr::str_replace_all(
      vignette_content,
      "\\(figure/",
      "("
    )
    readr::write_file(fixed_content, output_path)
  }

  cli::cli_alert_success("{vignette_name}.Rmd pre-compiled")
  return(invisible(TRUE))
}

vignette_names <- c("getting-started", "analysis-examples")
purrr::walk(vignette_names, precompile_vignette)

cli::cli_rule("All vignettes pre-compiled")
cli::cli_alert("Next steps:")
cli::cli_ul(c(
  "Review vignettes/*.Rmd files",
  "Run pkgdown::build_site() to test",
  "Commit both .Rmd.orig and .Rmd files"
))
