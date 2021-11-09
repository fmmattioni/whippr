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
