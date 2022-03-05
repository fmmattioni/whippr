# manual import of janitor::remove_empty() to avoid extra dependency

#' @title Remove empty rows and/or columns from a data.frame or matrix.
#'
#' @description Removes all rows and/or columns from a data.frame or matrix that
#'   are composed entirely of \code{NA} values.
#'
#' @param dat the input data.frame or matrix.
#' @param which one of "rows", "cols", or \code{c("rows", "cols")}.  Where no
#'   value of which is provided, defaults to removing both empty rows and empty
#'   columns, declaring the behavior with a printed message.
#' @param cutoff What fraction (>0 to <=1) of rows or columns must be empty to
#'   be removed?
#' @param quiet Should messages be suppressed (\code{TRUE}) or printed
#'   (\code{FALSE}) indicating the summary of empty columns or rows removed?
#' @return Returns the object without its missing rows or columns.
#' @keywords internal
remove_empty <- function(dat, which = c("rows", "cols"), cutoff=1) {
  if (missing(which) && !missing(dat)) {
    message("value for \"which\" not specified, defaulting to c(\"rows\", \"cols\")")
    which <- c("rows", "cols")
  }
  if ((sum(which %in% c("rows", "cols")) != length(which)) && !missing(dat)) {
    stop("\"which\" must be one of \"rows\", \"cols\", or c(\"rows\", \"cols\")")
  }
  if (length(cutoff) != 1) {
    stop("cutoff must be a single value")
  } else if (!is.numeric(cutoff)) {
    stop("cutoff must be numeric")
  } else if (cutoff <= 0 | cutoff > 1) {
    stop("cutoff must be >0 and <= 1")
  } else if (length(which) > 1 & cutoff != 1) {
    stop("cutoff must be used with only one of which = 'rows' or 'cols', not both")
  }
  if ("rows" %in% which) {
    # Using different code with cutoff = 1 vs cutoff != 1 to avoid possible
    # floating point errors.
    mask_keep <-
      if (cutoff == 1) {
        rowSums(is.na(dat)) != ncol(dat)
      } else {
        (rowSums(!is.na(dat))/ncol(dat)) > cutoff
      }
    dat <- dat[mask_keep, , drop = FALSE]
  }
  if ("cols" %in% which) {
    # Using different code with cutoff = 1 vs cutoff != 1 to avoid possible
    # floating point errors.
    mask_keep <-
      if (cutoff == 1) {
        colSums(is.na(dat)) != nrow(dat)
      } else {
        (colSums(!is.na(dat))/nrow(dat)) > cutoff
      }
    dat <- dat[, mask_keep, drop = FALSE]
  }
  dat
}
