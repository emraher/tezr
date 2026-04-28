# Shared test helpers for tezr test suite

# Silence all cli output (alerts, progress bars) for the current test scope
local_silence_cli <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    cli_alert_info = function(...) NULL,
    cli_alert_warning = function(...) NULL,
    cli_alert_success = function(...) NULL,
    cli_alert_danger = function(...) NULL,
    cli_progress_bar = function(...) NULL,
    cli_progress_update = function(...) NULL,
    cli_progress_done = function(...) NULL,
    .package = "cli",
    .env = env
  )
}
