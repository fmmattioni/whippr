#' Construct a new tibble with metadata
#'
#' @param .data A data frame
#' @param metadata Metadata to be passed along with the data
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
new_whippr_tibble <- function(.data, metadata) {
  if(!is.data.frame(.data))
    stop("You can only pass a data frame to this function.", call. = FALSE)

  whippr_tibble <- tibble::new_tibble(
    x = .data,
    nrow = nrow(.data),
    class = "whippr",
    read_data = metadata$read_data,
    metabolic_cart = metadata$metabolic_cart,
    data_status = metadata$data_status,
    time_column = metadata$time_column,
    vo2_column = metadata$vo2_column,
    test_type = metadata$test_type,
    processed_data = metadata$processed_data
  )

  tibble::validate_tibble(whippr_tibble)
  whippr_tibble
}

#' Whippr print method
#'
#' @param x A tibble with class 'whippr'
#' @param ... Extra arguments, not used.
#'
#' @export
print.whippr <- function(x, ...) {

  header <- paste("# Metabolic cart:", attr(x, "metabolic_cart"), "\n")

  if(!is.null(attr(x, "data_status")))
    header <- paste0(header, "# Data status: ", attr(x, "data_status"), "\n")

  if(!is.null(attr(x, "time_column")))
    header <- paste0(header, "# Time column: ", attr(x, "time_column"), "\n")

  if(!is.null(attr(x, "vo2_column")))
    header <- paste0(header, "# VO2 column: ", attr(x, "vo2_column"), "\n")

  if(!is.null(attr(x, "test_type")))
    header <- paste0(header, "# Test type: ", attr(x, "test_type"), "\n")

  cat(pillar::style_subtle(header))

  NextMethod()
}
