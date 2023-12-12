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

#' Model diagnostics
#'
#' Plots different model diagnostics for checking the model performance.
#'
#' @param .residuals_tbl The data retrived from \code{get_residuals()}.
#'
#' @return a patchwork object
#' @export
model_diagnostics <- function(.residuals_tbl) {
  ## Plots inspired both by the 'nlstools' and 'see' packages

  ## P1
  p1 <- .residuals_tbl %>%
    ggplot2::ggplot(ggplot2::aes(.resid)) +
    ggplot2::geom_density(fill = "skyblue", alpha = 0.5) +
    ggplot2::labs(
      title = "Non-Normality of Residuals",
      subtitle = "Distribution should look like a normal curve",
      x = "Residuals",
      y = "Density"
    ) +
    theme_whippr()

  ## P2
  p2 <- .residuals_tbl %>%
    ggplot2::ggplot(ggplot2::aes(sample = standardized_residuals)) +
    ggplot2::stat_qq(shape = 21, size = 4, fill = "skyblue") +
    ggplot2::stat_qq_line(color = "darkred", linewidth = 1) +
    ggplot2::labs(
      title = "Non-normality of Residuals and Outliers",
      subtitle = "Dots should be plotted along the line",
      y = "Standardized Residuals",
      x = "Theoretical Quantiles"
    ) +
    theme_whippr()

  ## P3
  p3 <- .residuals_tbl %>%
    ggplot2::ggplot(ggplot2::aes(.fitted, .resid)) +
    ggplot2::geom_point(shape = 21, size = 4, fill = "skyblue") +
    ggplot2::geom_hline(yintercept = 0, lty = "dashed", color = "darkred", linewidth = 1) +
    ggplot2::labs(
      title = "Homoscedasticity",
      subtitle = "Dots should be similar above and below the dashed line",
      x = "Fitted values",
      y = "Residuals"
    ) +
    theme_whippr()

  p4 <- .residuals_tbl %>%
    tidyr::drop_na(lag_residuals) %>%
    ggplot2::ggplot(ggplot2::aes(.resid, lag_residuals)) +
    ggplot2::geom_point(shape = 21, size = 4, fill = "skyblue") +
    ggplot2::geom_hline(yintercept = 0, lty = "dashed", color = "darkred", linewidth = 1) +
    ggplot2::labs(
      title = "Autocorrelation",
      subtitle = "Dots should be plotted randomly",
      x = "Residuals",
      y = "Residuals - i"
    ) +
    theme_whippr()

  out <- patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2)

  out
}
