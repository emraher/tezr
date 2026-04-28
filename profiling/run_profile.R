#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
})

profile_output_path <- "profiling/profile.Rprof"
profile_html_path <- "profiling/profile.html"
profile_libdir_path <- "profiling/profile_files"
by_self_path <- "profiling/by_self.csv"
by_total_path <- "profiling/by_total.csv"
summary_path <- "profiling/summary_str.txt"
results_fixture_path <- "tests/testthat/fixtures/results_tr.html"

dir.create("profiling", showWarnings = FALSE, recursive = TRUE)
results_html <- rvest::read_html(results_fixture_path)

profile_widget <- profvis::profvis(
  {
    parse_results_table(results_html)
  },
  interval = 0.01,
  prof_output = profile_output_path
)

summary_data <- utils::summaryRprof(profile_output_path)

utils::write.csv(summary_data$by.self, by_self_path)
utils::write.csv(summary_data$by.total, by_total_path)
writeLines(utils::capture.output(str(summary_data)), summary_path)

htmlwidgets::saveWidget(
  profile_widget,
  file = profile_html_path,
  selfcontained = FALSE,
  libdir = profile_libdir_path
)
