#' Perform average on second-by-second data
#'
#' This function performs either a bin- or a rolling-average on the interpolated data.
#' You must specify the \code{type} of the average before continuing.
#'
#' @param .data The second-by-second data retrieved from \code{interpolate()}.
#' @param type The type of the average to perform. Either \code{bin}, \code{rolling}, or \code{ensemble}.
#' @param bins If bin-average is chosen, here you can specify the size of the bin-average, in seconds. Default to 30-s bin-average.
#' @param bin_method Method for determining bin boundaries when \code{type = "bin"}.
#'   One of \code{"ceiling"} (default), \code{"round"}, or \code{"floor"}.
#'   \code{"ceiling"} is recommended as it ensures no data points are excluded
#'   from the analysis by always rounding up to the next bin boundary.
#' @param rolling_window If rolling-average is chosen, here you can specify the rolling-average window, in seconds. Default to 30-s rolling-average.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @details
#' Ensemble average is used in VO2 kinetics analysis, where a series of transitions from baseline to
#' the moderate/heavy/severe intensity-domain is ensembled averaged into a single 'bout' for further data processing.
#'
#' When using bin averaging, the \code{bin_method} parameter controls how time points are assigned to bins:
#' \itemize{
#'   \item \code{"ceiling"}: Rounds up to the next bin boundary (recommended)
#'   \item \code{"round"}: Rounds to the nearest bin boundary
#'   \item \code{"floor"}: Rounds down to the previous bin boundary
#' }
#'
#' @examples
#' \dontrun{
#' ## get file path from example data
#' path_example <- system.file("example_cosmed.xlsx", package = "whippr")
#'
#' ## read data
#' df <- read_data(path = path_example, metabolic_cart = "cosmed")
#'
#' ## interpolate and perform 30-s bin-average
#' df %>%
#'  interpolate() %>%
#'  perform_average(type = "bin", bins = 30)
#'
#' ## interpolate and perform 30-s rolling-average
#' df %>%
#'  interpolate() %>%
#'  perform_average(type = "rolling", rolling_window = 30)
#' }
perform_average <- function(.data, type = c("bin", "rolling", "ensemble"), bins = 30, bin_method = c("ceiling", "round", "floor"), rolling_window = 30) {
  if(missing(type))
    stop("You must specify the type of average you would like to perform.", call. = FALSE)

  type <- match.arg(type)
  bin_method <- match.arg(bin_method)

  class(.data) <- type

  UseMethod("perform_average", .data)
}

#' @export
perform_average.bin <- function(.data, type = c("bin", "rolling", "ensemble"), bins = 30, bin_method = c("ceiling", "round", "floor"), rolling_window = 30) {
  bin_method <- match.arg(bin_method)
  # select the appropriate rounding function
  rounding_func <- switch(bin_method,
                          "ceiling" = ceiling,
                          "round" = round,
                          "floor" = floor)
  ## first make sure data only contains numeric columns
  data_num <- .data %>%
    dplyr::select_if(is.numeric)

  out <- data_num %>%
    dplyr::group_by_at(1, function(x) rounding_func(x / bins) * bins) %>%
    dplyr::summarise_all(mean, na.rm = TRUE)

  metadata <- attributes(.data)
  metadata$data_status <- glue::glue("averaged data - {bins}-s bins")

  out <- new_whippr_tibble(out, metadata)

  out
}

#' @export
perform_average.rolling <- function(.data, type = c("bin", "rolling", "ensemble"), bins = 30, bin_method = c("ceiling", "round", "floor"), rolling_window = 30) {
  ## first make sure data only contains numeric columns
  data_num <- .data %>%
    dplyr::select_if(is.numeric)

  out <- data_num %>%
    zoo::rollmean(x = ., k = rolling_window) %>%
    dplyr::as_tibble()

  metadata <- attributes(.data)
  metadata$data_status <- glue::glue("averaged data - {rolling_window}-s rolling average")

  out <- new_whippr_tibble(out, metadata)

  out
}

#' @export
perform_average.ensemble <- function(.data, type = c("bin", "rolling", "ensemble"), bins = 30, bin_method = c("ceiling", "round", "floor"), rolling_window = 30) {
  ## first make sure data only contains numeric columns
  data_num <- .data %>%
    dplyr::select_if(is.numeric)

  out <- data_num %>%
    dplyr::group_by_at(1) %>%
    dplyr::summarise_all(mean, na.rm = TRUE)

  metadata <- attributes(.data)
  metadata$data_status <- "ensemble-averaged data"

  out <- new_whippr_tibble(out, metadata)

  out
}
