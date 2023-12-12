#' Linear method for detecting outliers from an incremental test
#'
#' Function for internal use only. It will not be exported.
#'
#' @param .data The data retrieved from `incremental_normalize()`.
#' @param time_column The name (quoted) of the column containing the time. Depending on the language of your system, this column might not be "t". Therefore, you may specify it here.
#' @param vo2_column The name (quoted) of the column containing the absolute oxygen uptake (VO2) data.
#' @param cleaning_level A numeric scalar between 0 and 1 giving the confidence level for the intervals to be calculated.
#'
#' @return a [tibble][tibble::tibble-package]
#' @keywords internal
outliers_linear <- function(
  .data,
  time_column,
  vo2_column,
  cleaning_level
) {
  ## set linear formula for each phase
  bsln_formula <- glue::glue("{vo2_column} ~ 1")
  incremental_formula <- glue::glue("{vo2_column} ~ {time_column}")

  out <- .data %>%
    ## separate each phase
    tidyr::nest(data = -protocol_phase) %>%
    dplyr::mutate(
      ## set the formula
      formula_model =
        dplyr::case_when(
          protocol_phase == "baseline" ~ bsln_formula,
          TRUE ~ incremental_formula
        ),
      ## set the model
      model = purrr::map2(
        .x = formula_model,
        .y = data,
        .f = ~ lm(formula = .x, data = .y)
      ),
      model_augmented = purrr::map(
        .x = model,
        .f = ~ broom::augment(.x)
      )
    ) %>%
    ## column not needed anymore
    dplyr::select(-formula_model) %>%
    ## delete additional columns to needed in augmented data
    dplyr::mutate(
      model_augmented = purrr::map(
        .x = model_augmented,
        .f = ~ dplyr::select(.x, -dplyr::any_of(c(time_column, vo2_column)))
      ),
      model_augmented = purrr::map2(
        .x = data,
        .y = model_augmented,
        .f = ~ dplyr::mutate(.y, x = .x[[time_column]], y = .x[[vo2_column]]) %>% dplyr::select(x, y, dplyr::everything())
      ),
      ## get confidence bands
      conf = purrr::map(
        .x = model,
        .f = ~ predict.lm(object = .x, interval = "confidence", level = cleaning_level) %>%
          dplyr::as_tibble() %>%
          dplyr::select(-1) %>%
          dplyr::rename_all(~ c("lwr_conf", "upr_conf"))
      ),
      ## get prediction bands
      pred = purrr::map(
        .x = model,
        .f = ~ suppressWarnings(predict.lm(object = .x, interval = "prediction", level = cleaning_level)) %>%
          dplyr::as_tibble() %>%
          dplyr::select(-1) %>%
          dplyr::rename_all(~ c("lwr_pred", "upr_pred"))
      )
    ) %>%
    dplyr::select(-model) %>%
    tidyr::unnest(cols = c(data:pred)) %>%
    dplyr::select(2:ncol(.), protocol_phase) %>%
    ## if VO2 is above or below the prediction bands, classify the breath as an outlier
    dplyr::mutate(
      outlier = ifelse(y >= lwr_pred & y <= upr_pred, "no", "yes")
    )

  out
}

#' Anomaly method for detecting outliers from an incremental test
#'
#' Function for internal use only. It will not be exported.
#'
#' @param .data The data retrieved from `incremental_normalize()`.
#' @param time_column The name (quoted) of the column containing the time. Depending on the language of your system, this column might not be "t". Therefore, you may specify it here.
#' @param vo2_column The name (quoted) of the column containing the absolute oxygen uptake (VO2) data.
#' @param cleaning_level A numeric scalar between 0 and 1 giving the confidence level for the intervals to be calculated.
#'
#' @return a [tibble][tibble::tibble-package]
#' @keywords internal
outliers_anomaly <- function(
  .data,
  time_column,
  vo2_column,
  cleaning_level
) {
  ## check if anomalize is installed
  rlang::check_installed("anomalize")

  ## set alpha
  alpha <- 1 - cleaning_level

  outliers_df <- .data %>%
    dplyr::mutate_at(1, function(x) x = lubridate::as_date(x)) %>%
    anomalize::time_decompose(target = !!rlang::sym(vo2_column), method = "stl", message = FALSE) %>%
    anomalize::anomalize(target = remainder, method = "iqr", alpha = alpha) %>%
    anomalize::time_recompose() %>%
    dplyr::select(anomaly:recomposed_l2) %>%
    dplyr::rename_all(~ c("outlier", "lwr_pred", "upr_pred")) %>%
    dplyr::mutate(outlier = stringr::str_to_lower(outlier))

  ## set time and vo2 columns as x and y
  ## this is to make simpler to plot it later on
  x_and_y <- .data %>%
    dplyr::select(!!rlang::sym(time_column), !!rlang::sym(vo2_column)) %>%
    dplyr::rename_all(~ c("x", "y"))

  out <- dplyr::bind_cols(.data, x_and_y, outliers_df)

  out
}
