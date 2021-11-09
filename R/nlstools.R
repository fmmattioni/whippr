#' Get residuals
#'
#' Computes residuals from the VO2 kinetics model.
#'
#' @param .model A model of class \code{nls}.
#'
#' @return a [tibble][tibble::tibble-package] containing the data passed to augment, and additional columns:
#' \item{.fitted}{The predicted response for that observation.}
#' \item{.resid}{The residual for a particular point.}
#' \item{standardized_residuals}{Standardized residuals.}
#' \item{sqrt_abs_standardized_residuals}{The sqrt of absolute value of standardized residuals.}
#' \item{lag_residuals}{The lag of the \code{.resid} column for plotting auto-correlation.}
#'
#' @export
get_residuals <- function(.model) {
  ## adapted from nlstools::nlsResiduals()

  sigma_model <- summary(.model)$sigma

  model_augmented <- broom::augment(.model)

  mean_residuals <- mean(model_augmented$.resid)

  out <- model_augmented %>%
    dplyr::mutate(standardized_residuals = (.resid - mean_residuals) / sigma_model,
                  sqrt_abs_standardized_residuals = abs(.resid / sigma_model),
                  sqrt_abs_standardized_residuals = sqrt(sqrt_abs_standardized_residuals),
                  lag_residuals = dplyr::lag(.resid))

  out
}
