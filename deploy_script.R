options(repos = c(CRAN = "https://cloud.r-project.org/"))

suppressPackageStartupMessages({
  if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")
  library(rsconnect)
})

deployApp(
  appDir = ".",
  appName = "leaguedata",
  forceUpdate = TRUE,
  launch.browser = FALSE,
  logLevel = "verbose"
)
