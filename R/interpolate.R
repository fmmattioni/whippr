#' Interpolate from breath-by-breath into second-by-second
#'
#' This function interpolates the data based on the time column. It takes the breath-by-breath data
#' and transforms it into second-by-second.
#'
#' @param .data Data retrieved from \code{read_data()}.
#' @param time_column The name (quoted) of the column containing the time. Default to "t".
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @importFrom stats approx
#'
#' @examples
#' \dontrun{
#' ## read data
#' df <- read_data(path = "~/Documents/data.xlsx", metabolic_cart = "cosmed")
#'
#' df_interpolated <- df %>%
#'    interpolate()
#' }
interpolate <- function(.data, time_column = "t") {
  ## check that column name exists
  if(!time_column %in% colnames(.data))
    stop("Column name does not exist in the data frame.", call. = FALSE)

  ## first make sure data only contains numeric columns
  data_num <- .data %>%
    dplyr::select_if(is.numeric)

  lapply(data_num, function (i) approx(
    x = data_num[[ {{ time_column }} ]],
    y = i,
    xout = seq(min(data_num[[ {{ time_column }} ]]), max(data_num[[ {{ time_column }} ]], na.rm = TRUE), 1)
  )$y
  ) %>%
    dplyr::as_tibble()
}
