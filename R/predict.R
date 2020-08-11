#' Extract confidence and prediction bands
#'
#' It extracts confidence and prediction bands from the \code{nls} model. It is used only for data cleaning.
#'
#' @param .data The nornalized data retrieved from \code{normalize_transitions()}.
#' @param time_column The name (quoted) of the column containing the time. Depending on the language of your system, this column might not be "t". Therefore, you may specify it here.  Default to "t".
#' @param vo2_column The name (quoted) of the column containing the absolute oxygen uptake (VO2) data. Default to 'VO2'.
#' @param cleaning_level A numeric scalar between 0 and 1 giving the confidence level for the intervals to be calculated.
#' @param cleaning_baseline_fit A character indicating what kind of fit to perform for each baseline. Either 'linear' or 'exponential'.
#'
#' @return a [tibble][tibble::tibble-package] containing the following columns:
#' \item{x}{The provided time data.}
#' \item{y}{The provided VO2 data.}
#' \item{.fitted}{The predicted response for that observation.}
#' \item{.resid}{The residual for a particular point.}
#' \item{lwr_conf}{Lower limit of the confidence band.}
#' \item{upr_conf}{Upper limit of the confidence band.}
#' \item{lwr_pred}{Lower limit of the prediction band.}
#' \item{upr_pred}{Upper limit of the prediction band.}
#'
#' @export
#'
#' @importFrom utils tail
predict_bands <- function(
  .data,
  time_column = "t",
  vo2_column = "VO2",
  cleaning_level = 0.95,
  cleaning_baseline_fit = c("linear", "exponential")
) {


  # check arguments ---------------------------------------------------------
  cleaning_baseline_fit <- match.arg(cleaning_baseline_fit)

  # prepare data ------------------------------------------------------------
  data_bsln <- .data %>%
    dplyr::filter(phase == "baseline")

  data_transition <- .data %>%
    dplyr::filter(phase == "transition")


  # baseline fit ------------------------------------------------------------
  bands_bsln <- predict_bands_baseline(
    .data = data_bsln,
    time_column = time_column,
    vo2_column = vo2_column,
    cleaning_level = cleaning_level,
    cleaning_baseline_fit = cleaning_baseline_fit
  )

  # define bsln value -------------------------------------------------------
  baseline_value <- tail(bands_bsln$.fitted, 10) %>%
    mean()

  # define model depending on the intensity domain --------------------------
  ## set starting values
  start_Amp <- max(data_transition[[vo2_column]]) - baseline_value
  start_TD <- 360
  start_tau <- 30

  formula_model <- glue::glue("{vo2_column} ~ {baseline_value} + Amp * (1 - exp(-({time_column} - TD)/tau))")

  cleaning_model <- minpack.lm::nlsLM(
    formula = formula_model,
    data = data_transition,
    start = list(Amp = start_Amp, TD = start_TD, tau = start_tau)
  )

  bands_transition <- predict_bands_transition(
    .data = data_transition,
    time_column = time_column,
    vo2_column = vo2_column,
    cleaning_level = cleaning_level,
    cleaning_model = cleaning_model
  )

  out <- dplyr::bind_rows(bands_bsln, bands_transition)

  out

}

#' Extract confidence and prediction bands for the baseline phase
#'
#' @param .data The nornalized data retrieved from \code{normalize_transitions()}. The data should be filtered to only the 'baseline' phase before passing to the function.
#' @param time_column The name (quoted) of the column containing the time. Depending on the language of your system, this column might not be "t". Therefore, you may specify it here.  Default to "t".
#' @param vo2_column The name (quoted) of the column containing the absolute oxygen uptake (VO2) data. Default to 'VO2'.
#' @param cleaning_level A numeric scalar between 0 and 1 giving the confidence level for the intervals to be calculated.
#' @param cleaning_baseline_fit A character indicating what kind of fit to perform for each baseline. Either 'linear' or 'exponential'.
#'
#' @return a [tibble][tibble::tibble-package] containing the following columns:
#' \item{x}{The provided time data.}
#' \item{y}{The provided VO2 data.}
#' \item{.fitted}{The predicted response for that observation.}
#' \item{.resid}{The residual for a particular point.}
#' \item{lwr_conf}{Lower limit of the confidence band.}
#' \item{upr_conf}{Upper limit of the confidence band.}
#' \item{lwr_pred}{Lower limit of the prediction band.}
#' \item{upr_pred}{Upper limit of the prediction band.}
#'
#' @export
#'
#' @importFrom stats lm predict.lm
#' @importFrom utils head
predict_bands_baseline <- function(
  .data,
  time_column,
  vo2_column,
  cleaning_level,
  cleaning_baseline_fit
) {

  if(cleaning_baseline_fit == "linear") {

    # linear fit with slope = 0
    linear_formula <- glue::glue("{vo2_column} ~ 1")
    model_bsln <- lm(linear_formula, data = .data)

    # prediction with prediction bands
    out <- .data %>%
      dplyr::select(1) %>%
      dplyr::bind_cols(broom::augment(model_bsln)) %>%
      dplyr::rename_at(1:2, ~ c("x", "y")) %>%
      dplyr::select(x:.resid) %>%
      dplyr::bind_cols(dplyr::as_tibble(predict.lm(model_bsln, interval = "confidence", level = cleaning_level))) %>%
      dplyr::rename(
        lwr_conf = lwr,
        upr_conf = upr
      ) %>%
      dplyr::select(-fit) %>%
      dplyr::bind_cols(dplyr::as_tibble(suppressWarnings(predict.lm(model_bsln, interval = "prediction", level = cleaning_level)))) %>%
      dplyr::rename(
        lwr_pred = lwr,
        upr_pred = upr
      ) %>%
      dplyr::select(-fit) %>%
      dplyr::mutate(
        outlier = ifelse(y >= lwr_pred & y <= upr_pred, "no", "yes")
      )
  } else {

    ## set starting values
    baseline_value <- head(.data[[vo2_column]], 3) %>% mean()
    start_Amp <- min(.data[[vo2_column]]) - max(.data[[vo2_column]])
    start_TD <- 10
    start_tau <- 30

    formula_model <- glue::glue("{vo2_column} ~ {baseline_value} + Amp * (1 - exp(-({time_column} - TD)/tau))")

    model_bsln_exp <- minpack.lm::nlsLM(formula_model,
                                        data = .data,
                                        start=list(Amp = start_Amp, TD = start_TD, tau = start_tau))

    out <- predict_bands_transition(
      .data = .data,
      time_column = time_column,
      vo2_column = vo2_column,
      cleaning_level = cleaning_level,
      cleaning_model = model_bsln_exp
    )

  }

  out

}

#' Extract confidence and prediction bands for the transition phase
#'
#' @param .data The nornalized data retrieved from \code{normalize_transitions()}. The data should be filtered to only the 'transition' phase before passing to the function.
#' @param time_column The name (quoted) of the column containing the time. Depending on the language of your system, this column might not be "t". Therefore, you may specify it here.  Default to "t".
#' @param vo2_column The name (quoted) of the column containing the absolute oxygen uptake (VO2) data. Default to 'VO2'.
#' @param cleaning_level A numeric scalar between 0 and 1 giving the confidence level for the intervals to be calculated.
#' @param cleaning_model The \code{nls} model to retrieve the bands from.
#'
#' @return a [tibble][tibble::tibble-package] containing the following columns:
#' \item{x}{The provided time data.}
#' \item{y}{The provided VO2 data.}
#' \item{.fitted}{The predicted response for that observation.}
#' \item{.resid}{The residual for a particular point.}
#' \item{lwr_conf}{Lower limit of the confidence band.}
#' \item{upr_conf}{Upper limit of the confidence band.}
#' \item{lwr_pred}{Lower limit of the prediction band.}
#' \item{upr_pred}{Upper limit of the prediction band.}
#'
#' @export
#'
#' @importFrom stats coef deriv qt vcov
predict_bands_transition <- function(
  .data,
  time_column,
  vo2_column,
  cleaning_level,
  cleaning_model
) {
  ## this code is adapted from: http://sia.webpopix.org/nonlinearRegression.html#confidence-intervals-for-the-model-parameters

  summary_model <- summary(cleaning_model)
  augmented_model <- broom::augment(cleaning_model)

  residuals_model <- augmented_model %>%
    dplyr::select(.resid) %>%
    dplyr::pull()

  cleaning_level <- 1 - cleaning_level

  ## first step is to compute the variance
  fgh2 <- deriv(summary_model$formula, c("Amp", "TD", "tau"), function(Amp, TD, tau, t){} )

  x_new <- .data[[time_column]]
  y <- .data[[vo2_column]]

  beta_est <- coef(cleaning_model)

  f_new <- fgh2(beta_est[1], beta_est[2], beta_est[3], x_new)

  g_new <- attr(f_new, "gradient")
  V_beta <- vcov(cleaning_model)
  GS <- rowSums((g_new %*% V_beta) * g_new)

  ## confidence bands
  degrees_freedom <- summary_model$df[2]
  delta_f <- sqrt(GS) * qt(1 - cleaning_level / 2, degrees_freedom)

  ## prediction bands
  sigma_est <- summary_model$sigma
  delta_y <- sqrt(GS + sigma_est ^ 2) * qt(1 - cleaning_level / 2, degrees_freedom)

  out <- dplyr::tibble(
    x = x_new,
    y = y,
    .fitted = f_new,
    .resid = residuals_model,
    lwr_conf = f_new - delta_f,
    upr_conf = f_new + delta_f,
    lwr_pred = f_new - delta_y,
    upr_pred = f_new + delta_y
  ) %>%
    dplyr::mutate(
      outlier = ifelse(y >= lwr_pred & y <= upr_pred, "no", "yes")
    )

  out
}
