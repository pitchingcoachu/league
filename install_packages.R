options(repos = c(CRAN = "https://cloud.r-project.org/"))

pkgs <- c(
  "rsconnect",
  "shiny", "dplyr", "DT", "ggplot2",
  "readr", "stringr", "curl",
  "DBI", "RPostgres", "digest"
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
