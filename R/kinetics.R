#' Perform VO2 kinetics fitting
#'
#' Performs the fitting process for the VO2 kinetics analysis. At this point, the data should already have been cleaned (outliers removed) and processed
#' (interpolated, time-aligned, ensembled-averaged, and bin-averaged).
#'
#' @param .data_processed The data retrived from `process_data()`.
#' @param intensity_domain The exercise-intensity domain that the test was performed. Either *moderate*, *heavy*, or *severe*.
#' @param fit_level A numeric scalar between 0 and 1 giving the confidence level for the parameter estimates in the final VO2 kinetics fit. Default to `0.95`.
#' @param fit_phase_1_length The length of the phase I that you wish to exclude from the final exponential fit, in seconds. See  `VO2 kinetics` section in `?vo2_kinetics` for more details.
#' @param fit_baseline_length The length the baseline to perform the final linear fit, in seconds. See `VO2 kinetics` section `?vo2_kinetics` for more details.
#' @param fit_transition_length The length of the transition to perform the final exponential fit, in seconds. See `VO2 kinetics` section `?vo2_kinetics` for more details.
#' @param verbose A boolean indicating whether messages should be printed in the console. Default to `TRUE`.
#' @param ... Additional arguments when fiting VO2 kinetics in the heavy- or severe-intensity domains. Arguments may be the following:
#' \describe{
#'  \item{\code{TODO}}{}
#' }
#'
#' @details
#' See `?vo2_kinetics` for details.
#'
#' @return a [tibble][tibble::tibble-package] containing one row and the nested columns:
#' \item{data_fitted}{The data containing the time and VO2 columns, as well as the fitted data and its residuals for each data point.}
#' \item{model}{A `nls` object. The model used in the VO2 kinetics fitting.}
#' \item{model_summary}{The tidied summary of the `model`.}
#' \item{model_residuals}{The residuals of the `model`.}
#' \item{plot_model}{The final plot of the fitted `model`.}
#' \item{plot_residuals}{The residuals plot for the `model` diagnostics.}
#'
#' @export
perform_kinetics <- function(
  .data_processed,
  intensity_domain = c("moderate", "heavy", "severe"),
  fit_level = 0.95,
  fit_phase_1_length,
  fit_baseline_length,
  fit_transition_length,
  verbose = TRUE,
  ...
) {

  if(min(.data_processed[[1]]) >= 0)
    stop("It looks like you forgot to normalize the time column. At this point, the time at baseline should have negative values.", call. = FALSE)

  if(missing(fit_phase_1_length))
    stop("You must specify the length of the Phase 1 that you would like to exclude from the fit. See ?vo2_kinetics for more details.", call. = FALSE)

  if(missing(fit_baseline_length))
    stop("You must specify the length of the baseline for the final fit. See ?vo2_kinetics for more details.", call. = FALSE)

  if(missing(fit_transition_length))
    stop("You must specify the length of the transition for the final fit. See ?vo2_kinetics for more details.", call. = FALSE)

  if(any(c(fit_phase_1_length, fit_baseline_length, fit_transition_length) < 0))
    stop("All the `fit_*` arguments must be positive values.", call. = FALSE)

  intensity_domain <- match.arg(intensity_domain)

  if(intensity_domain != "moderate")
    intensity_domain <- "heavy"

  class(.data_processed) <- intensity_domain

  UseMethod("perform_kinetics", .data_processed)
}

#' @export
#'
#' @importFrom stats lm
perform_kinetics.moderate <- function(
  .data_processed,
  intensity_domain = c("moderate", "heavy", "severe"),
  fit_level = 0.95,
  fit_phase_1_length,
  fit_baseline_length,
  fit_transition_length,
  verbose = TRUE,
  ...
) {
  # set time and VO2 columns ------------------------------------------------
  time_column <- "time"
  vo2_column <- "VO2"

  # prepare data ------------------------------------------------------------
  data_bsln <- .data_processed %>%
    dplyr::filter_at(1, function(x) x >= -fit_baseline_length & x<= 0)

  data_transition <- .data_processed %>%
    dplyr::filter_at(1, function(x) x >= fit_phase_1_length & x<= fit_transition_length)

  # baseline fit ------------------------------------------------------------

  ## linear fit with slope = 0
  formula_bsln <- glue::glue("{vo2_column} ~ 1")
  model_bsln <- lm(formula = formula_bsln, data = data_bsln)

  ## predicted values
  res_bsln <- data_bsln %>%
    dplyr::select(1) %>%
    dplyr::bind_cols(broom::augment(model_bsln)) %>%
    dplyr::select(1:.resid)

  # define bsln value -------------------------------------------------------
  baseline_value <- unique(res_bsln$.fitted)


  # transition fit ----------------------------------------------------------

  ## set starting values
  start_Amp <- max(data_transition[[vo2_column]]) - baseline_value
  start_TD <- 20
  start_tau <- 30

  formula_transition <- glue::glue("{vo2_column} ~ {baseline_value} + Amp * (1 - exp(-({time_column} - TD)/tau))")

  model_transition <- minpack.lm::nlsLM(
    formula = formula_transition,
    data = data_transition,
    start = list(Amp = start_Amp, TD = start_TD, tau = start_tau)
  )

  # summaries ---------------------------------------------------------------

  ## model summary tidied

  ### baseline
  res_bsln <- broom::tidy(model_bsln, conf.int = TRUE, conf.level = fit_level) %>%
    dplyr::mutate(term = "baseline")

  ### transition
  res_transition_tidy <- broom::tidy(model_transition)
  res_transition_conf <- nlstools::confint2(model_transition, level = fit_level) %>%
    dplyr::as_tibble(rownames = NA) %>%
    dplyr::add_rownames() %>%
    dplyr::rename_all(~ c("term", "conf.low", "conf.high"))

  res_transition <- dplyr::left_join(res_transition_tidy, res_transition_conf, by = "term")

  ### summary
  res_summary <- dplyr::bind_rows(res_bsln, res_transition)

  ## model residuals
  model_residuals <- get_residuals(model_transition)

  ## model augmented
  aug_baseline <- broom::augment(model_bsln) %>%
    dplyr::mutate(t = data_bsln[[ {{time_column}} ]]) %>%
    dplyr::select(t, dplyr::everything()) %>%
    dplyr::rename_at(1, ~ {{time_column}})

  aug_transition <- broom::augment(model_transition)

  ## final df with fitted values for both bsln and transition
  res_total <- dplyr::bind_rows(aug_baseline, aug_transition)

  out <- dplyr::tibble(
    data_fitted = list(res_total),
    model = list(model_transition),
    model_summary = list(res_summary),
    model_residuals = list(model_residuals)
  )

  out
}

#' @export
#'
#' @importFrom stats lm
perform_kinetics.heavy <- function(
  .data_processed,
  intensity_domain = c("moderate", "heavy", "severe"),
  fit_level = 0.95,
  fit_phase_1_length,
  fit_baseline_length,
  fit_transition_length,
  verbose = TRUE,
  ...
) {
  stop("I am sorry, this is not implemented yet.", call. = FALSE)
}

#' Process data for VO2 kinetics fitting
#'
#' It removes the outliers detected through `detect_outliers()`, interpolates each transition,
#' ensemble-averages all the transitions into one, performs a bin-average, and normalizes the time column
#' (time zero will indicate the end of baseline and the start of the transition phase).
#'
#' @param .data_outliers The data retrived from `detect_outliers()`.
#' @param protocol_baseline_length The length of the baseline (in seconds).
#' @param fit_bin_average The bin average to be performed for the final fit.
#'
#' @details
#' TODO
#'
#' @return a [tibble][tibble::tibble-package] with the time-aligned, ensembled-averaged, and bin-averaged data.
#' @export
process_data <- function(
  .data_outliers,
  protocol_baseline_length,
  fit_bin_average
) {
  pre_process <- .data_outliers %>%
    ## remove outliers
    dplyr::filter(outlier == "no") %>%
    ## remove columns not needed anymore
    dplyr::select(-c(cleaning_baseline_fit:outlier)) %>%
    ## interpolate each transition separately
    tidyr::nest(data = -transition)

  ## if 1 transition, there is no need to ensemble-average
  n_transitions <- nrow(pre_process)

  if(n_transitions > 1) {
    out <- pre_process %>%
      dplyr::rowwise() %>%
      dplyr::mutate(data = list(interpolate(data))) %>%
      dplyr::ungroup() %>%
      tidyr::unnest(cols = data) %>%
      dplyr::select(2:ncol(.)) %>%
      ## ensemble-average transitions
      perform_average(.data = ., type = "ensemble") %>%
      ## perform bin-average for fitting
      perform_average(.data = ., type = "bin", bins = fit_bin_average) %>%
      normalize_time(.data = ., protocol_baseline_length = protocol_baseline_length)
  } else {
    out <- pre_process %>%
      dplyr::rowwise() %>%
      dplyr::mutate(data = list(interpolate(data))) %>%
      dplyr::ungroup() %>%
      tidyr::unnest(cols = data) %>%
      dplyr::select(2:ncol(.)) %>%
      ## perform bin-average for fitting
      perform_average(.data = ., type = "bin", bins = fit_bin_average) %>%
      normalize_time(.data = ., protocol_baseline_length = protocol_baseline_length)
  }

  out
}
