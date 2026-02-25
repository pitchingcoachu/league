# app_minimal.R
# Runs the same app code as app.R with minimal runtime mode enabled.
# This preserves page aesthetics/behavior while skipping non-required module mounts.

Sys.setenv(
  LEAGUE_MINIMAL = "1",
  LEAGUE_SOURCE_ONLY = "1"
)

source("app.R", local = TRUE)
app
