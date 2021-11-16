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
interpolate <- function(.data) {
  ## first make sure data only contains numeric columns
  data_num <- dplyr::select(.data, where(is.numeric))

  keep_rows <- rowSums(is.na(data_num)) != ncol(data_num)
  df_numeric_rows <- data_num[keep_rows, , drop = FALSE]

  keep_columns <- colSums(!is.na(df_numeric_rows)) > 0
  df_ready <- df_numeric_rows[, keep_columns, drop = FALSE]

  suppressWarnings({
    out <- lapply(df_ready, function (i) approx(
      x = df_ready[[1]],
      y = i,
      xout = seq(min(df_ready[[1]]), max(df_ready[[1]], na.rm = TRUE), 1)
    )$y
    ) %>%
      dplyr::as_tibble()
  })

  out
}
