#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
})

source("pitch_data_service.R")

perf_now <- function() as.numeric(proc.time()[["elapsed"]])
perf_line <- function(label, sec, rows = NA_integer_) {
  rows_txt <- if (is.finite(rows)) sprintf(" rows=%s", format(as.integer(rows), big.mark = ",")) else ""
  cat(sprintf("[perf] %s | sec=%.3f%s\n", label, sec, rows_txt))
}

startup_logger <- function(msg) cat(sprintf("[startup] %s\n", msg))

t0 <- perf_now()
res <- tryCatch(
  load_pitch_data_with_backend(
    local_data_dir = file.path(getwd(), "data"),
    school_code = "",
    startup_logger = startup_logger
  ),
  error = function(e) e
)
t1 <- perf_now()

if (inherits(res, "error") || is.null(res) || is.null(res$data)) {
  msg <- if (inherits(res, "error")) res$message else "No data returned from backend."
  cat(sprintf("[perf] backend_load_failed | sec=%.3f msg=%s\n", t1 - t0, msg))
  quit(status = 1)
}

pitch_data <- res$data
perf_line("backend_load", t1 - t0, nrow(pitch_data))

if (!nrow(pitch_data)) {
  cat("[perf] No rows available for filter benchmarks.\n")
  quit(status = 0)
}

if (!"Date" %in% names(pitch_data)) pitch_data$Date <- as.Date(NA)
if (!"SessionType" %in% names(pitch_data)) pitch_data$SessionType <- NA_character_
if (!"PitcherTeam" %in% names(pitch_data)) pitch_data$PitcherTeam <- NA_character_
if (!"BatterTeam" %in% names(pitch_data)) pitch_data$BatterTeam <- NA_character_

team_codes <- unique(c(as.character(pitch_data$PitcherTeam), as.character(pitch_data$BatterTeam)))
team_codes <- team_codes[!is.na(team_codes) & nzchar(trimws(team_codes))]
sample_team <- if (length(team_codes)) team_codes[[1]] else ""

date_vals <- suppressWarnings(as.Date(pitch_data$Date))
date_vals <- date_vals[!is.na(date_vals)]
if (length(date_vals)) {
  end_date <- max(date_vals)
  start_date <- end_date - 14
} else {
  end_date <- as.Date("2100-01-01")
  start_date <- as.Date("1900-01-01")
}

t2 <- perf_now()
df_session <- pitch_data %>%
  dplyr::filter(SessionType %in% c("Live", "Bullpen"))
t3 <- perf_now()
perf_line("filter_session", t3 - t2, nrow(df_session))

t4 <- perf_now()
df_date <- df_session %>%
  dplyr::mutate(.DateTmp = suppressWarnings(as.Date(Date))) %>%
  dplyr::filter(.DateTmp >= start_date, .DateTmp <= end_date) %>%
  dplyr::select(-.DateTmp)
t5 <- perf_now()
perf_line("filter_date_14d", t5 - t4, nrow(df_date))

if (nzchar(sample_team)) {
  t6 <- perf_now()
  df_team <- df_date %>%
    dplyr::filter(as.character(PitcherTeam) == sample_team | as.character(BatterTeam) == sample_team)
  t7 <- perf_now()
  perf_line(sprintf("filter_team_%s", sample_team), t7 - t6, nrow(df_team))
}

t8 <- perf_now()
agg <- df_date %>%
  dplyr::group_by(Pitcher, TaggedPitchType) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")
t9 <- perf_now()
perf_line("group_pitcher_pitchtype", t9 - t8, nrow(agg))

cat("[perf] profile complete\n")
