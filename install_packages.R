options(repos = c(CRAN = "https://cloud.r-project.org/"))

pkgs <- c(
  "rsconnect",
  "shiny", "shinyjs", "dplyr", "DT",
  "ggplot2", "ggiraph", "gridExtra", "patchwork", "hexbin", "MASS",
  "plotly", "readr", "stringr", "curl", "lubridate", "httr2",
  "DBI", "RPostgres", "digest", "rlang",
  "colourpicker", "memoise", "shinymanager", "RColorBrewer"
)

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = c("Depends", "Imports", "LinkingTo"))
  }
}

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop(
    sprintf("Failed to install required package(s): %s", paste(missing, collapse = ", ")),
    call. = FALSE
  )
}

cat("Package install complete\n")
