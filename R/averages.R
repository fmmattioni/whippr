#' Perform average on second-by-second data
#'
#' This function performs either a bin- or a rolling-average on the interpolated data.
#' You must specify the \code{type} of the average before continuing.
#'
#' @param .data The second-by-second data retrieved from \code{interpolate()}.
#' @param time_column The name (quoted) of the column containing the time. Default to "t".
#' @param type The type of the average to perform. Either \code{bin} or \code{rolling}.
#' @param bins If bin-average is chosen, here you can specify the size of the bin-average, in seconds. Default to 30-s bin-average.
#' @param rolling_window If rolling-average is chosen, here you can specify the rolling-average window, in seconds. Default to 30-s rolling-average.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' \dontrun{
#' ## read data
#' df <- read_data(path = "~/Documents/data.xlsx", metabolic_cart = "cosmed")
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
perform_average <- function(.data, time_column = "t", type = c("bin", "rolling"), bins = 30, rolling_window = 30) {
  if(missing(type))
    stop("You must specify the type of average you would like to perform.", call. = FALSE)

  type <- match.arg(type)

  class(.data) <- type

  UseMethod("perform_average", .data)
}

#' @export
perform_average.bin <- function(.data, time_column = "t", type = c("bin", "rolling"), bins = 30, rolling_window = 30) {
  ## check that column name exists
  if(!time_column %in% colnames(.data))
    stop("Column name does not exist in the data frame.", call. = FALSE)

  ## first make sure data only contains numeric columns
  data_num <- .data %>%
    dplyr::select_if(is.numeric)

  out <- data_num %>%
    ## this is required to simplify the next step, in case the time column is not named "t"
    dplyr::rename(t = {{ time_column }}) %>%
    dplyr::group_by(t = round(t / bins) * bins) %>%
    dplyr::summarise_all(mean) %>%
    dplyr::rename_at(1, {{ time_column }})

  out
}

#' @export
perform_average.rolling <- function(.data, time_column = "t", type = c("bin", "rolling"), bins = 30, rolling_window = 30) {
  ## check that column name exists
  if(!time_column %in% colnames(.data))
    stop("Column name does not exist in the data frame.", call. = FALSE)

  ## first make sure data only contains numeric columns
  data_num <- .data %>%
    dplyr::select_if(is.numeric)

  out <- data_num %>%
    ## this is required to simplify the next step, in case the time column is not named "t"
    dplyr::rename(t = {{ time_column }}) %>%
    dplyr::select(t, dplyr::everything()) %>%
    zoo::rollmean(x = ., k = rolling_window) %>%
    dplyr::as_tibble() %>%
    dplyr::rename_at(1, {{ time_column }})

  out
}
