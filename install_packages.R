options(repos = c(CRAN = "https://cloud.r-project.org/"))

pkgs <- c(
  "shiny", "dplyr", "DT", "ggplot2", "plotly",
  "readr", "stringr", "curl", "lubridate",
  "DBI", "RPostgres", "digest", "rlang"
)

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
}

cat("Package install complete\n")
