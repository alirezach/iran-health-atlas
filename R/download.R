# Download helpers ------------------------------------------------------------

#' Download a file unless a non-empty copy already exists
#' @noRd
download_if_needed <- function(url, dest, overwrite = FALSE, timeout = 7200) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(dest) && file.size(dest) > 0 && !overwrite) return(dest)
  old <- options(timeout = max(timeout, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  tmp <- paste0(dest, ".part")
  status <- utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  if (status != 0 || !file.exists(tmp) || file.size(tmp) == 0) {
    stop("Download failed: ", url)
  }
  file.rename(tmp, dest)
  dest
}

#' First token of a remote .md5 file, or NA when offline
#' @noRd
remote_md5 <- function(url) {
  tryCatch({
    line <- readLines(url, n = 1L, warn = FALSE)
    strsplit(trimws(line), " ", fixed = TRUE)[[1]][1]
  }, error = function(e) NA_character_)
}

file_md5 <- function(path) unname(tools::md5sum(path))

#' geoBoundaries API metadata for a level (ADM1, ADM2, ...)
#' @noRd
geoboundaries_meta <- function(api, level) {
  url <- gsub("{level}", level, api, fixed = TRUE)
  jsonlite::fromJSON(url)
}
