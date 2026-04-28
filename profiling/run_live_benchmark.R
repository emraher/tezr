#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
})

get_arg_value <- function(args, name, default_value = NULL) {
  prefix <- paste0("--", name, "=")
  matching_args <- args[startsWith(args, prefix)]
  if (length(matching_args) == 0) {
    return(default_value)
  }

  return(sub(prefix, "", matching_args[[1]], fixed = TRUE))
}

parse_logical_arg <- function(value, default_value = FALSE) {
  if (is.null(value)) {
    return(default_value)
  }

  normalized_value <- tolower(value)
  if (normalized_value %in% c("true", "t", "1", "yes", "y")) {
    return(TRUE)
  }
  if (normalized_value %in% c("false", "f", "0", "no", "n")) {
    return(FALSE)
  }

  stop("Invalid logical argument: ", value, call. = FALSE)
}

print_usage <- function() {
  cat(
    paste(
      "Usage: Rscript profiling/run_live_benchmark.R [options]",
      "",
      "Options:",
      "  --keyword=<text>        Keyword to search (default: iklim değişikliği)",
      "  --reps=<n>              Number of repetitions per scenario (default: 3)",
      "  --ignore_cache=<bool>   Pass ignore_cache to search functions (default: TRUE)",
      "  --output=<path>         Output CSV path (default: profiling/live_benchmark_<timestamp>.csv)",
      "  --help                  Show this help message",
      sep = "\n"
    )
  )
}

run_benchmark_case <- function(case_name, case_function, repetitions) {
  case_results <- vector("list", repetitions)

  for (rep_index in seq_len(repetitions)) {
    gc()

    benchmark_error <- NULL
    benchmark_result <- NULL
    timing <- system.time({
      benchmark_result <- tryCatch(
        case_function(),
        error = function(error) {
          benchmark_error <<- conditionMessage(error)
          return(NULL)
        }
      )
    })

    case_results[[rep_index]] <- data.frame(
      scenario = case_name,
      repetition = rep_index,
      elapsed = unname(timing[["elapsed"]]),
      user = unname(timing[["user.self"]]),
      system = unname(timing[["sys.self"]]),
      status = if (is.null(benchmark_error)) "ok" else "error",
      rows = if (is.null(benchmark_result)) {
        NA_integer_
      } else {
        nrow(benchmark_result)
      },
      error = if (is.null(benchmark_error)) "" else benchmark_error,
      stringsAsFactors = FALSE
    )

    cat(
      sprintf(
        "[%s] rep %d/%d: status=%s elapsed=%.2fs rows=%s\n",
        case_name,
        rep_index,
        repetitions,
        case_results[[rep_index]]$status,
        case_results[[rep_index]]$elapsed,
        ifelse(
          is.na(case_results[[rep_index]]$rows),
          "NA",
          case_results[[rep_index]]$rows
        )
      )
    )
  }

  return(do.call(rbind, case_results))
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  if ("--help" %in% args) {
    print_usage()
    return(invisible(NULL))
  }

  keyword <- get_arg_value(args, "keyword", "iklim değişikliği")
  repetitions <- suppressWarnings(as.integer(get_arg_value(args, "reps", "3")))
  if (is.na(repetitions) || repetitions < 1L) {
    stop("--reps must be a positive integer", call. = FALSE)
  }

  ignore_cache <- parse_logical_arg(
    get_arg_value(args, "ignore_cache", "true"),
    default_value = TRUE
  )
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  default_output_path <- file.path(
    "profiling",
    paste0("live_benchmark_", timestamp, ".csv")
  )
  output_path <- get_arg_value(args, "output", default_output_path)

  benchmark_cases <- list(
    basic_2000 = function() {
      return(search_basic(
        keyword = keyword,
        max_search_results = 2000,
        ignore_cache = ignore_cache
      ))
    },
    basic_all = function() {
      return(search_basic(
        keyword = keyword,
        max_search_results = Inf,
        ignore_cache = ignore_cache
      ))
    },
    advanced_all = function() {
      return(search_advanced(
        keyword = keyword,
        max_search_results = Inf,
        ignore_cache = ignore_cache
      ))
    }
  )

  benchmark_rows <- lapply(
    names(benchmark_cases),
    function(case_name) {
      run_benchmark_case(
        case_name = case_name,
        case_function = benchmark_cases[[case_name]],
        repetitions = repetitions
      )
    }
  )
  benchmark_data <- do.call(rbind, benchmark_rows)

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(benchmark_data, output_path, row.names = FALSE)
  cat("Saved benchmark results to:", output_path, "\n")

  return(invisible(benchmark_data))
}

main()
