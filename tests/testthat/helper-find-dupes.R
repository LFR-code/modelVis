# helper-find-dupes.R
# Utility to find duplicate chunk labels within a single render tree
# (a top-level Rmd plus every child= document it pulls in, recursively).
# The same label reused across two independent top-level templates is
# not a conflict -- they are never knit together -- so labels are
# tagged with which tree they belong to and only compared within it.

.chunk_labels_in_file <- function(path) {
  lines <- readLines(con = path, warn = FALSE)
  chunk_pat <- "^```[{]r[[:space:]]+([^,} ]+).*$"
  idx <- grep(pattern = chunk_pat, x = lines)
  sub(pattern = chunk_pat, replacement = "\\1", x = lines[idx])
}

.child_files_in <- function(path, rmd_dir) {
  lines <- readLines(con = path, warn = FALSE)
  pat <- 'child[[:space:]]*=[[:space:]]*system[.]file[(]"rmd",[[:space:]]*"([^"]+)"'
  hits <- regmatches(x = lines, m = regexpr(pattern = pat, text = lines))
  children <- sub(pattern = pat, replacement = "\\1", x = hits)
  file.path(rmd_dir, children)
}

find_chunk_labels <- function() {
  rmd_dir <- system.file(
    "rmd", package = "modelVis"
  )
  rmd_files <- list.files(
    path = rmd_dir, pattern = "[.]Rmd$",
    full.names = TRUE
  )

  all_children <- unlist(lapply(
    X = rmd_files, FUN = .child_files_in, rmd_dir = rmd_dir
  ))
  top_level <- rmd_files[!basename(rmd_files) %in% basename(all_children)]

  all_labels <- character(0)
  label_files <- character(0)
  label_trees <- character(0)
  for (top in top_level) {
    tree_files <- c(top, .child_files_in(top, rmd_dir))
    for (f in tree_files) {
      lbls <- .chunk_labels_in_file(f)
      all_labels <- c(all_labels, lbls)
      label_files <- c(label_files, rep(basename(f), length(lbls)))
      label_trees <- c(label_trees, rep(basename(top), length(lbls)))
    }
  }
  data.frame(
    label = all_labels,
    file  = label_files,
    tree  = label_trees,
    stringsAsFactors = FALSE
  )
}
