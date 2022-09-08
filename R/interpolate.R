#' Interpolate data from breath-by-breath into second-by-second
#'
#' This function interpolates the data based on the time column. It takes the breath-by-breath data
#' and transforms it into second-by-second.
#'
#' @param .data Data retrieved from \code{read_data()}.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @importFrom stats approx
#'
#' @examples
#' \dontrun{
#' ## get file path from example data
#' path_example <- system.file("example_cosmed.xlsx", package = "whippr")
#'
#' ## read data
#' df <- read_data(path = path_example, metabolic_cart = "cosmed")
#'
#' df %>%
#'  interpolate()
#' }
interpolate <- function(.data) {
  ## first make sure data only contains numeric columns
  data_num <- .data %>%
    dplyr::select_if(is.numeric) %>%
    remove_empty(dat = ., which = c("rows", "cols"))

  suppressWarnings({
    out <- lapply(data_num, function (i) approx(
      x = data_num[[1]],
      y = i,
      xout = seq(min(data_num[[1]]), max(data_num[[1]], na.rm = TRUE), 1)
    )$y
    ) %>%
      dplyr::as_tibble()
  })


  metadata <- attributes(.data)
  metadata$data_status <- "interpolated data"

  out <- new_whippr_tibble(out, metadata)

  out
}
