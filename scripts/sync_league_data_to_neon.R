#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(curl)
  library(readr)
  library(stringr)
})

source("pitch_data_service.R")

FTP_HOST <- "ftp.trackmanbaseball.com"
FTP_USER <- "LStateU"
FTP_PASS <- "6!Bh%dJ!_y"
FTP_USERPWD <- paste0(FTP_USER, ":", FTP_PASS)

DATE_CUTOFF <- as.Date("2026-02-13")
LOCAL_DATA_DIR <- file.path("data")
LOCAL_V3_DIR <- file.path(LOCAL_DATA_DIR, "v3")
LEAGUE_CODE <- "LEAGUE"

dir.create(LOCAL_V3_DIR, recursive = TRUE, showWarnings = FALSE)

list_ftp_names <- function(path) {
  url <- paste0("ftp://", FTP_HOST, path)
  tryCatch({
    h <- curl::new_handle(userpwd = FTP_USERPWD)
    curl::handle_setopt(h, ftp_use_epsv = FALSE, dirlistonly = TRUE)
    txt <- rawToChar(curl::curl_fetch_memory(url, handle = h)$content)
    out <- trimws(gsub("\r", "", strsplit(txt, "\n", fixed = TRUE)[[1]]))
    out[nzchar(out)]
  }, error = function(e) character(0))
}

download_ftp_file <- function(remote_path, local_path) {
  dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(local_path)) return(TRUE)
  url <- paste0("ftp://", FTP_HOST, remote_path)
  tryCatch({
    h <- curl::new_handle(userpwd = FTP_USERPWD)
    curl::handle_setopt(h, ftp_use_epsv = FALSE)
    curl::curl_download(url, destfile = local_path, mode = "wb", handle = h)
    TRUE
  }, error = function(e) {
    message("Download failed: ", remote_path, " :: ", e$message)
    FALSE
  })
}

is_allowed_file <- function(name) {
  nm <- tolower(trimws(name))
  grepl("\\.csv$", nm) &&
    !grepl("unverified|playerpositioning|battracking", nm)
}

scan_v3_candidates <- function() {
  out <- character(0)
  years <- as.character(2026:as.integer(format(Sys.Date(), "%Y")))
  for (yy in years) {
    months <- list_ftp_names(paste0("/v3/", yy, "/"))
    months <- months[grepl("^\\d{2}$", months)]
    for (mm in months) {
      days <- list_ftp_names(paste0("/v3/", yy, "/", mm, "/"))
      days <- days[grepl("^\\d{2}$", days)]
      for (dd in days) {
        d <- as.Date(sprintf("%s-%s-%s", yy, mm, dd))
        if (is.na(d) || d < DATE_CUTOFF) next
        base <- paste0("/v3/", yy, "/", mm, "/", dd, "/")
        names <- list_ftp_names(base)
        if ("CSV" %in% names) {
          names <- list_ftp_names(paste0(base, "CSV/"))
          base <- paste0(base, "CSV/")
        }
        files <- names[vapply(names, is_allowed_file, logical(1))]
        if (length(files)) {
          out <- c(out, paste0(base, files))
        }
      }
    }
  }
  unique(out)
}

local_name_for_remote <- function(remote_path) {
  m <- stringr::str_match(remote_path, "/v3/(\\d{4})/(\\d{2})/(\\d{2})/")
  yy <- ifelse(is.na(m[1, 2]), "0000", m[1, 2])
  mm <- ifelse(is.na(m[1, 3]), "00", m[1, 3])
  dd <- ifelse(is.na(m[1, 4]), "00", m[1, 4])
  paste0("v3_", yy, "_", mm, "_", dd, "_", basename(remote_path))
}

sync_league <- function() {
  con <- pitch_data_db_connect()
  on.exit(tryCatch(DBI::dbDisconnect(con), error = function(e) NULL), add = TRUE)
  ensure_pitch_data_schema(con)

  remote_files <- scan_v3_candidates()
  if (!length(remote_files)) {
    message("No remote league CSV candidates found.")
    return(invisible(0L))
  }

  message("Remote candidates: ", length(remote_files))
  total_rows <- 0L
  ok <- 0L
  fail <- 0L
  skipped <- 0L

  for (i in seq_along(remote_files)) {
    rp <- remote_files[[i]]
    lp <- file.path(LOCAL_V3_DIR, local_name_for_remote(rp))
    if (!download_ftp_file(rp, lp)) {
      fail <- fail + 1L
      next
    }
    one <- tryCatch({
      DBI::dbBegin(con)
      n <- sync_csv_file_to_neon(con, lp, school_code = LEAGUE_CODE)
      DBI::dbCommit(con)
      n
    }, error = function(e) {
      tryCatch(DBI::dbRollback(con), error = function(e2) NULL)
      message("Sync failed: ", rp, " :: ", e$message)
      structure(0L, skipped = FALSE, failed = TRUE)
    })
    if (isTRUE(attr(one, "failed"))) {
      fail <- fail + 1L
    } else {
      ok <- ok + 1L
      if (isTRUE(attr(one, "skipped"))) skipped <- skipped + 1L
      total_rows <- total_rows + as.integer(one)
    }
    if (i %% 25L == 0L || i == length(remote_files)) {
      message(sprintf("Progress %d/%d | ok=%d skipped=%d failed=%d rows=%d",
                      i, length(remote_files), ok, skipped, fail, total_rows))
    }
  }

  message(sprintf("League sync complete | total=%d ok=%d skipped=%d failed=%d rows=%d",
                  length(remote_files), ok, skipped, fail, total_rows))
  invisible(total_rows)
}

if (!interactive()) {
  sync_league()
}
