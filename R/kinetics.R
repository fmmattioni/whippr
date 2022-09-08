#' VO2 kinetics
#'
#' It performs the whole process of the VO2 kinetics data analysis, which includes:
#' data cleaning (`detect_outliers()`); outliers removal, interpolation, ensemble-averaging transitions and bin-avering final dataset (`process_data()`),
#' and modelling VO2 kinetics (`perform_kinetics()`). This function is a general function that will call these separate functions.
#' You can also call each one of them separately if you want.
#'
#' @param .data Data retrieved from `read_data()`.
#' @param intensity_domain The exercise-intensity domain that the test was performed. Either *moderate*, *heavy*, or *severe*.
#' @param vo2_column The name (quoted) of the column containing the absolute oxygen uptake (VO2) data. Default to `"VO2"`.
#' @param protocol_n_transitions Number of transitions performed.
#' @param protocol_baseline_length The length of the baseline (in seconds).
#' @param protocol_transition_length The length of the transition (in seconds).
#' @param cleaning_level A numeric scalar between 0 and 1 giving the confidence level for the intervals to be calculated during the data cleaning process. Breaths lying outside the prediction bands will be excluded. Default to `0.95`.
#' @param cleaning_baseline_fit A vector of the same length as the number in `protocol_n_transitions`, indicating what kind of fit to perform for each baseline. Either *linear* or *exponential*.
#' @param fit_level A numeric scalar between 0 and 1 giving the confidence level for the parameter estimates in the final VO2 kinetics fit. Default to `0.95`.
#' @param fit_bin_average The bin average to be performed for the final fit.
#' @param fit_phase_1_length The length of the phase I that you wish to exclude from the final exponential fit, in seconds. See  `VO2 kinetics` section for more details.
#' @param fit_baseline_length The length the baseline to perform the final linear fit, in seconds. See `VO2 kinetics` section for more details.
#' @param fit_transition_length The length of the transition to perform the final exponential fit, in seconds. See `VO2 kinetics` section for more details.
#' @param verbose A boolean indicating whether messages should be printed in the console. Default to `TRUE`.
#' @param ... Additional arguments passed to `perform_kinetics()` when fitting VO2 kinetics in the heavy- or severe-intensity domains. See `?perform_kinetics` for more details.
#'
#' @details
#' The function is a wrapper of smaller functions and has important arguments:
#'
#' * \strong{protocol_} = sets arguments related to the protocol used.
#' * \strong{cleaning_} = sets arguments related to data cleaning.
#' * \strong{fit_} = sets arguments related to VO2 kinetics fitting.
#'
#' The function works like the following sequence:
#'
#' **`vo2_kinetics( )`**:
#'
#' * `detect_outliers( )` = separates the data into the number of transitions indicated,
#' and fits each baseline and transition phase indiviudally, retrieving the predictions bands for the level indicated.
#' Then it recognizes breaths lying outside the prediciton bands and flag them as outliers.
#' * `plot_outliers( )` = plots each transition identifying outliers.
#' * `process_data( )` = It removes the outliers detected through `detect_outliers()`, interpolates each transition,
#' ensemble-averages all the transitions into one, performs a bin-average, and normalizes the time column
#' (time zero will indicate the end of baseline and the start of the transition phase).
#' * `perform_kinetics( )` = performs the VO2 kinetics fitting based on the \strong{fit_} parameters given.
#' It also calculates the residuals, and plots the final fit as well as residuals for model diagnostics.
#'
#' @details # VO2 kinetics
#'
#' VO2 kinetics, described as the rate of adjustment of the oxidative energy system to an
#' instantaneous increase in the energy demand, is exponential in nature, and it is described by the
#' oxygen uptake (VO2) time-constant (\eqn{\tau}VO2) (Murias, Spencer and Paterson (2014); Poole and Jones (2011)).
#'
#' VO2 kinetics analysis provides understanding of the mechanisms that regulate the rate at which oxidative
#' phosphorylation adapts to step changes in exercise intensities and ATP requirement. This is usually accomplished
#' by performing step transitions from a baseline intensity to a higher work rate in either the **moderate-**, **heavy-**, or
#' **severe-intensity domain** (Murias et al., 2011).
#'
#' Three distinct phases may be observed in the VO2 response during on-transient exercise:
#'
#' **Phase I**: also termed as the cardiodynamic phase, it represents the circulatory transit delay
#' on the VO2 response as a result of the increase in the pulmonary blood flow that does not reflect the increase
#' in oxygen extraction in the active muscles. The time-window of the Phase I is determined in the **`fit_phase_1_length`** argument, which will be internally passed into the `perform_kinetics()` function.
#'
#' **Phase II**: also termed as the primary component, represents the exponential increase in VO2
#' related to the continued increase in pulmonary and muscle blood flow. The Phase II is described by the time-constant parameter (\eqn{\tau})
#' in the mono-exponential model (see below), and it is defined as the duration of time (in seconds) for the VO2 response
#' to increase to 63% of the required steady-state.
#'
#' **Phase III**: represents the steady-state phase of the VO2 response
#' during moderate-intensity exercise.
#'
#' ## Moderate-intensity domain
#' The on-transient response from baseline to a transition within the **moderate-intensity domain**
#' is analyzed using a **mono-exponential model**:
#' \deqn{VO_{2\left(t\right)}=baseline+amplitude\cdot\left(1-e^{^{-\frac{\left(t-TD\right)}{tau}}}\right)}{%
#' VO2(t) = baseline + amplitude * (-exp(-(t-TD)/\tau))}
#'
#' where:
#'
#' * `VO2(t)` = the oxygen uptake at any given time.
#' * `baseline` = the oxygen uptake associated with the baseline phase.
#' * `amplitude` = the steady-state increase increase in oxygen uptake above `baseline`.
#' * `TD` = the time delay.
#' * \eqn{\tau} = the time constant defined as the duration of time for the oxygen uptake to increase to 63% of the steady-state increase.
#'
#' The baseline value in the mono-exponential model is a **fixed** value and pre-determined
#' as the mean of the VO2 response (i.e., linear model with the slope set as zero) during the baseline phase.
#' The time window of the baseline period is determined in the **`fit_baseline_length`** argument, which will be internally passed into the `perform_kinetics()` function.
#'
#' Diverse exercise protocols exist to determine VO2 kinetics in the moderate-intensity domain.
#' Usually, the protocol consists of multiple transitions (typically 3 or 4) from a baseline exercise-intensity to an exercise-intensity
#' below the gas exchange threshold (typically the power output associated with 90% of the gas exchange threshold). Bbaseline and
#' transition phases are usually performed for 6 minutes each. The reason that 6 minutes is done for each phase is to give enough time for both
#' to reach a steady-state response:
#'
#' For example, for each multiple of the time-constant (\eqn{\tau}), VO2 increases by 63% of the
#' difference between the previous \eqn{\tau} and the required steady-state.
#' This means:
#'
#' * `1` \eqn{\tau} `= 63%` \eqn{\Delta}.
#' * `2` \eqn{\tau} `= 86%` \eqn{\Delta} `[100% - 63% = 37%; (37% x 63%) + 63% = 86%]`.
#' * `3` \eqn{\tau} `= 95%` \eqn{\Delta} `[100% - 86% = 14%; (14% x 63%) + 86% = 95%]`.
#' * `4` \eqn{\tau} `= 98%` \eqn{\Delta} `[100% - 95% = 5%; (5% x 63%) + 95% = 98%]`.
#'
#' In practical terms, let's imagine that a given participant has a **\eqn{\tau} = 60 seconds**. This means that this person
#' would need **240 seconds** (`4 x 60`) to reach **steady-state** (98% of the response) in the **moderate-intensity domain**. This would leave other
#' 120 seconds (2 minutes) of transition, so the protocol of performing 6-min transitions makes sure enough time is given.
#'
#' Now let's imagine that another person has a **\eqn{\tau} = 20 seconds**. This means that this person
#' would need **80 seconds** (`4 x 20`) to reach **steady-state** (98% of the response) in the **moderate-intensity domain**.
#'
#' Given that there is enough time to reach a VO2 steady-state response with 6 minutes of transition, that means that for the final fit
#' (when the transitions were cleaned, ensembled-averaged, and bin-averaged) there is no need to include the whole 6 minutes of the transition.
#' This strategy avoids superfluous sections of the steady‐state data, thus maximizing the quality of the fit during the exercise on‐transient (Bell et al., 2001).
#' This may be specified through the **`fit_transition_length`** argument, which will be internally passed into the `perform_kinetics()` function.
#'
#' As for bin-averages in the final fit, usually the data are averaged into 5-s or 10-s bins, 5-s being the most common (Keir et al., 2014).
#' This may be specified through the **`fit_bin_average`** argument, which will be internally passed into the `process_data()` function.
#'
#' ## Heavy- and severe-intensity domains
#' TODO
#'
#' @references
#' Bell, C., Paterson, D. H., Kowalchuk, J. M., Padilla, J., & Cunningham, D. A. (2001). A comparison of modelling techniques used to characterise oxygen uptake kinetics during the on-transient of exercise. Experimental Physiology, 86(5), 667-676.
#'
#' Keir, D. A., Murias, J. M., Paterson, D. H., & Kowalchuk, J. M. (2014). Breath‐by‐breath pulmonary O2 uptake kinetics: effect of data processing on confidence in estimating model parameters. Experimental physiology, 99(11), 1511-1522.
#'
#' Murias, J. M., Spencer, M. D., & Paterson, D. H. (2014). The critical role of O2 provision in the dynamic adjustment of oxidative phosphorylation. Exercise and sport sciences reviews, 42(1), 4-11.
#'
#' Murias, J. M., Spencer, M. D., Kowalchuk, J. M., & Paterson, D. H. (2011). Influence of phase I duration on phase II VO2 kinetics parameter estimates in older and young adults. American Journal of Physiology-regulatory, integrative and comparative physiology, 301(1), R218-R224.
#'
#' Poole, D. C., & Jones, A. M. (2011). Oxygen uptake kinetics. Comprehensive Physiology, 2(2), 933-996.
#'
#' @return a [tibble][tibble::tibble-package] containing one row and the nested columns:
#' \item{data_outliers}{The raw data containing additional columns that identify breaths as outliers.}
#' \item{plot_outliers}{A `patchwork` object to display outliers from every transition.}
#' \item{data_processed}{The processed data (time-aligned, ensembled-averaged, and bin-averaged).}
#' \item{data_fitted}{The data containing the time and VO2 columns, as well as the fitted data and its residuals for each data point.}
#' \item{model}{A `nls` object. The model used in the VO2 kinetics fitting.}
#' \item{model_summary}{The tidied summary of the `model`.}
#' \item{model_residuals}{The residuals of the `model`.}
#' \item{plot_model}{The final plot of the fitted `model`.}
#' \item{plot_residuals}{The residuals plot for the `model` diagnostics.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## get file path from example data
#' path_example <- system.file("example_cosmed.xlsx", package = "whippr")
#'
#' ## read data
#' df <- read_data(path = path_example, metabolic_cart = "cosmed", time_column = "t")
#'
#' ## VO2 kinetics analysis
#' results_kinetics <- vo2_kinetics(
#'   .data = df,
#'   intensity_domain = "moderate",
#'   vo2_column = "VO2",
#'   protocol_n_transitions = 3,
#'   protocol_baseline_length = 360,
#'   protocol_transition_length = 360,
#'   cleaning_level = 0.95,
#'   cleaning_baseline_fit = c("linear", "exponential", "exponential"),
#'   fit_level = 0.95,
#'   fit_bin_average = 5,
#'   fit_phase_1_length = 20,
#'   fit_baseline_length = 120,
#'   fit_transition_length = 240,
#'   verbose = TRUE
#' )
#' }
vo2_kinetics <- function(
  .data,
  intensity_domain = c("moderate", "heavy", "severe"),
  vo2_column = "VO2",
  protocol_n_transitions,
  protocol_baseline_length,
  protocol_transition_length,
  cleaning_level = 0.95,
  cleaning_baseline_fit,
  fit_level = 0.95,
  fit_bin_average,
  fit_phase_1_length,
  fit_baseline_length,
  fit_transition_length,
  verbose = TRUE,
  ...
) {

  if(missing(.data) | "rlang_fake_data_pronoun" %in% class(.data))
    stop("Did you forget to pass the raw data to the `.data` argument?", call. = FALSE)

  if(is.null(attributes(.data)$read_data))
    stop("It looks like you did not read your data with the `read_data()` function. Make sure you use it before continuing.", call. = FALSE)

  intensity_domain <- match.arg(intensity_domain)

  if(intensity_domain != "moderate")
    stop(glue::glue("I am sorry, VO2 kinetics in the {intensity_domain}-intensity domain is not implemented yet."), call. = FALSE)

  if(verbose)
    cli::cli_rule(center = cli::col_red(" * V\u0307O\u2082 kinetics analysis * "))

  # first, rename time and VO2 columns --------------------------------------
  ## this is for simplicity since this function wraps many other functions
  time_column <- attributes(.data)$time_column
  attributes(.data)$time_column <- "t"

  .data <- .data %>%
    dplyr::rename(
      "t" = {{ time_column }},
      "VO2" = {{ vo2_column }}
    )

  # detect outliers ---------------------------------------------------------
  data_outliers <- detect_outliers(
    .data = .data,
    test_type = "kinetics",
    vo2_column = "VO2",
    protocol_n_transitions = protocol_n_transitions,
    protocol_baseline_length = protocol_baseline_length,
    protocol_transition_length = protocol_transition_length,
    cleaning_level = cleaning_level,
    cleaning_baseline_fit = cleaning_baseline_fit,
    verbose = verbose
  )

  ## get outliers plot
  outliers_plot <- plot_outliers(.data = data_outliers)

  if(verbose) {
    usethis::ui_done("Processing data...")
    usethis::ui_done(usethis::ui_field("      \u2514\u2500 Removing outliers"))
    usethis::ui_done(usethis::ui_field("      \u2514\u2500 Interpolating each transition"))
    usethis::ui_done(usethis::ui_field("      \u2514\u2500 Ensemble-averaging transitions"))
    usethis::ui_done(usethis::ui_field("      \u2514\u2500 Performing {fit_bin_average}-s bin averages"))
  }
  # process data ------------------------------------------------------------
  data_processed <- process_data(
    .data_outliers = data_outliers,
    protocol_baseline_length = protocol_baseline_length,
    fit_bin_average = fit_bin_average
  )


  if(verbose) {
    usethis::ui_done("Fitting data...")
    usethis::ui_done(usethis::ui_field("      \u2514\u2500 Fitting baseline"))
    usethis::ui_done(usethis::ui_field("      \u2514\u2500 Fitting transition"))
    usethis::ui_done(usethis::ui_field("      \u2514\u2500 Calculating residuals"))
    usethis::ui_done(usethis::ui_field("      \u2514\u2500 Preparing plots"))
  }
  # perform vo2 kinetics fit ------------------------------------------------
  data_kinetics <- perform_kinetics(
    .data_processed = data_processed,
    intensity_domain = intensity_domain,
    fit_level = fit_level,
    fit_phase_1_length = fit_phase_1_length,
    fit_baseline_length = fit_baseline_length,
    fit_transition_length = fit_transition_length,
    verbose =  verbose,
    ... = ...
  )

  if(verbose)
    cli::cli_rule(center = cli::col_blue(" * DONE * "))

  out <- dplyr::tibble(
    data_outliers = list(data_outliers),
    plot_outliers = list(outliers_plot),
    data_processed = list(data_processed)
  ) %>%
    dplyr::bind_cols(data_kinetics)

  out
}

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


  # check data --------------------------------------------------------------
  if(is.null(attributes(.data_processed)$processed_data))
    stop("It looks like you did not process your data with `process_data()`", call. = FALSE)

  # set time and VO2 columns ------------------------------------------------
  time_column <- attributes(.data_processed)$time_column ## defined in read_data()
  vo2_column <- attributes(.data_processed)$vo2_column ## defined in detect_outliers()

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
    tibble::rownames_to_column() %>%
    dplyr::rename_all(~ c("term", "conf.low", "conf.high"))

  res_transition <- dplyr::left_join(res_transition_tidy, res_transition_conf, by = "term")

  ### summary
  res_summary <- dplyr::bind_rows(res_bsln, res_transition)

  ## model residuals
  model_residuals <- get_residuals(model_transition)
  ## plot model residuals
  plot_residuals <- model_diagnostics(model_residuals)

  ## model augmented
  aug_baseline <- broom::augment(model_bsln) %>%
    dplyr::mutate(t = data_bsln[[ {{time_column}} ]]) %>%
    dplyr::select(t, dplyr::everything()) %>%
    dplyr::rename_at(1, ~ {{time_column}})

  aug_transition <- broom::augment(model_transition)

  ## final df with fitted values for both bsln and transition
  res_total <- dplyr::bind_rows(aug_baseline, aug_transition)

  data_for_plot <- res_total %>%
    dplyr::left_join(.data_processed, ., by = c({{time_column}}, {{vo2_column}})) %>%
    dplyr::filter_at(1, function(x) x >= -fit_baseline_length)

  # fit plot ----------------------------------------------------------------
  p1 <- data_for_plot %>%
    ggplot2::ggplot(ggplot2::aes(!!rlang::sym(time_column), !!rlang::sym(vo2_column))) +
    ggplot2::geom_point(shape = 21, size = 4, fill = "white") +
    ggplot2::geom_line(ggplot2::aes(!!rlang::sym(time_column), .fitted), color = "red") +
    theme_whippr()

  p2 <- data_for_plot %>%
    ggplot2::ggplot(ggplot2::aes(!!rlang::sym(time_column), .resid)) +
    ggplot2::geom_line(alpha = 0.4) +
    theme_whippr()

  out_plot <- patchwork::wrap_plots(p1, p2, ncol = 1, heights = c(8, 1))

  out <- dplyr::tibble(
    data_fitted = list(res_total),
    model = list(model_transition),
    model_summary = list(res_summary),
    model_residuals = list(model_residuals),
    plot_model = list(out_plot),
    plot_residuals = list(plot_residuals)
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
      dplyr::mutate(data = purrr::map(data, interpolate)) %>%
      tidyr::unnest(cols = data) %>%
      dplyr::select(2:ncol(.)) %>%
      ## ensemble-average transitions
      perform_average(.data = ., type = "ensemble") %>%
      ## perform bin-average for fitting
      perform_average(.data = ., type = "bin", bins = fit_bin_average) %>%
      normalize_time(.data = ., protocol_baseline_length = protocol_baseline_length)
  } else {
    out <- pre_process %>%
      dplyr::mutate(data = purrr::map(data, interpolate)) %>%
      tidyr::unnest(cols = data) %>%
      dplyr::select(2:ncol(.)) %>%
      ## perform bin-average for fitting
      perform_average(.data = ., type = "bin", bins = fit_bin_average) %>%
      normalize_time(.data = ., protocol_baseline_length = protocol_baseline_length)
  }

  metadata <- attributes(.data_outliers)
  metadata$data_status <- glue::glue("processed data - {fit_bin_average}-s bin averaged")
  metadata$processed_data <- TRUE

  out <- new_whippr_tibble(out, metadata)

  out
}
