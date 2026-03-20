# tidy_array.R
# Exported helper functions for converting R arrays into
# long-format data.frames with standardised column names.
# Used by extractor functions in model packages.

#' Tidy a time-series vector or 1D array
#'
#' Converts a named numeric vector (indexed by year) into
#' a long-format data.frame.
#'
#' @param x Numeric vector with year names or indices.
#' @param years Integer vector of year labels.
#' @param value_col Name for the value column.
#' @return A data.frame with columns \code{year} and
#'   \code{value_col}.
#' @export
mv_tidy_ts <- function(x, years, value_col = "est") {
  n <- length(x)
  if (length(years) != n) {
    stop(
      "mv_tidy_ts: length of x (", n,
      ") != length of years (", length(years), ")",
      call. = FALSE
    )
  }
  df <- data.frame(
    year = years,
    val  = as.numeric(x),
    stringsAsFactors = FALSE
  )
  names(df)[names(df) == "val"] <- value_col
  df
}


#' Tidy a matrix (2D array)
#'
#' Converts a matrix into long-format, using dimnames or
#' supplied labels for rows and columns.
#'
#' @param x A matrix.
#' @param row_name Name for the row dimension column.
#' @param col_name Name for the column dimension column.
#' @param row_labels Labels for rows (default: rownames).
#' @param col_labels Labels for columns (default: colnames).
#' @param value_col Name for the value column.
#' @return A data.frame in long format.
#' @export
mv_tidy_mat <- function(x, row_name = "row",
                        col_name = "col",
                        row_labels = NULL,
                        col_labels = NULL,
                        value_col = "est") {
  if (is.null(row_labels)) {
    row_labels <- if (!is.null(rownames(x))) {
      rownames(x)
    } else {
      seq_len(nrow(x))
    }
  }
  if (is.null(col_labels)) {
    col_labels <- if (!is.null(colnames(x))) {
      colnames(x)
    } else {
      seq_len(ncol(x))
    }
  }
  grid <- expand.grid(
    row = row_labels,
    col = col_labels,
    stringsAsFactors = FALSE
  )
  names(grid) <- c(row_name, col_name)
  grid[[value_col]] <- as.numeric(x)
  grid
}


#' Tidy a 3D array
#'
#' Converts a 3D array into long-format data.frame.
#'
#' @param x A 3D array.
#' @param dim_names Character vector of length 3 giving
#'   column names for each dimension.
#' @param dim_labels List of length 3 giving labels for
#'   each dimension. NULL elements use dimnames or seq.
#' @param value_col Name for the value column.
#' @return A data.frame in long format.
#' @export
mv_tidy_3d <- function(x,
                       dim_names = c("d1", "d2", "d3"),
                       dim_labels = list(NULL, NULL, NULL),
                       value_col = "est") {
  d <- dim(x)
  labs <- vector(mode = "list", length = 3L)
  for (i in seq_len(3L)) {
    if (!is.null(dim_labels[[i]])) {
      labs[[i]] <- dim_labels[[i]]
    } else if (!is.null(dimnames(x)[[i]])) {
      labs[[i]] <- dimnames(x)[[i]]
    } else {
      labs[[i]] <- seq_len(d[i])
    }
  }
  grid <- expand.grid(
    d1 = labs[[1]],
    d2 = labs[[2]],
    d3 = labs[[3]],
    stringsAsFactors = FALSE
  )
  names(grid) <- dim_names
  grid[[value_col]] <- as.numeric(x)
  grid
}


#' Tidy a 4D array
#'
#' @param x A 4D array.
#' @param dim_names Character vector of length 4.
#' @param dim_labels List of length 4.
#' @param value_col Name for the value column.
#' @return A data.frame in long format.
#' @export
mv_tidy_4d <- function(x,
                       dim_names = c("d1", "d2", "d3",
                                     "d4"),
                       dim_labels = list(NULL, NULL, NULL,
                                         NULL),
                       value_col = "est") {
  d <- dim(x)
  labs <- vector(mode = "list", length = 4L)
  for (i in seq_len(4L)) {
    if (!is.null(dim_labels[[i]])) {
      labs[[i]] <- dim_labels[[i]]
    } else if (!is.null(dimnames(x)[[i]])) {
      labs[[i]] <- dimnames(x)[[i]]
    } else {
      labs[[i]] <- seq_len(d[i])
    }
  }
  grid <- expand.grid(
    d1 = labs[[1]],
    d2 = labs[[2]],
    d3 = labs[[3]],
    d4 = labs[[4]],
    stringsAsFactors = FALSE
  )
  names(grid) <- dim_names
  grid[[value_col]] <- as.numeric(x)
  grid
}


#' Tidy a 5D array
#'
#' @param x A 5D array.
#' @param dim_names Character vector of length 5.
#' @param dim_labels List of length 5.
#' @param value_col Name for the value column.
#' @return A data.frame in long format.
#' @export
mv_tidy_5d <- function(x,
                       dim_names = paste0("d", 1:5),
                       dim_labels = vector("list", 5L),
                       value_col = "est") {
  d <- dim(x)
  labs <- vector(mode = "list", length = 5L)
  for (i in seq_len(5L)) {
    if (!is.null(dim_labels[[i]])) {
      labs[[i]] <- dim_labels[[i]]
    } else if (!is.null(dimnames(x)[[i]])) {
      labs[[i]] <- dimnames(x)[[i]]
    } else {
      labs[[i]] <- seq_len(d[i])
    }
  }
  grid <- expand.grid(
    d1 = labs[[1]],
    d2 = labs[[2]],
    d3 = labs[[3]],
    d4 = labs[[4]],
    d5 = labs[[5]],
    stringsAsFactors = FALSE
  )
  names(grid) <- dim_names
  grid[[value_col]] <- as.numeric(x)
  grid
}
