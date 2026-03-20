# helper-find-dupes.R
# Utility to find duplicate chunk labels.

find_chunk_labels <- function() {
  rmd_dir <- system.file(
    "rmd", package = "modelVis"
  )
  rmd_files <- list.files(
    path = rmd_dir, pattern = "[.]Rmd$",
    full.names = TRUE
  )
  all_labels <- character(0)
  label_files <- character(0)
  for (f in rmd_files) {
    lines <- readLines(con = f, warn = FALSE)
    chunk_pat <- "^```[{]r[[:space:]]+([^,} ]+)"
    idx <- grep(pattern = chunk_pat, x = lines)
    for (i in idx) {
      lbl <- sub(
        pattern = chunk_pat,
        replacement = "\\1",
        x = lines[i]
      )
      all_labels <- c(all_labels, lbl)
      label_files <- c(label_files, basename(f))
    }
  }
  data.frame(
    label = all_labels,
    file  = label_files,
    stringsAsFactors = FALSE
  )
}
