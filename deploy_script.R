options(repos = c(CRAN = "https://cloud.r-project.org/"))

suppressPackageStartupMessages({
  if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")
  library(rsconnect)
})

# Ensure deployment machine has all required packages installed first.
source("install_packages.R")

required_pkgs <- c("ggiraph", "shiny", "dplyr", "DT", "ggplot2")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    sprintf(
      "Missing required package(s) after install step: %s",
      paste(missing_pkgs, collapse = ", ")
    ),
    call. = FALSE
  )
}

deployApp(
  appDir = ".",
  appPrimaryDoc = "app_minimal.R",
  appName = "leaguedata",
  forceUpdate = TRUE,
  launch.browser = FALSE,
  logLevel = "verbose"
)
