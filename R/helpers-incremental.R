#' Work rate for a ramp-incremental test
#'
#' This function produces the work rate throughout a ramp-incremental test given the procotol
#'
#' @param .data The data with recognized protocol phases
#' @param baseline_intensity The baseline intensity
#' @param ramp_increase The ramp increase, in watts per minute
#'
#' @return a [tibble][tibble::tibble-package]
#' @keywords internal
work_rate_ramp <- function(
  .data,
  baseline_intensity,
  ramp_increase
) {
  model <- lm(work_rate ~ time, data = data.frame(work_rate = c(baseline_intensity, baseline_intensity + ramp_increase), time = c(0, 60)))

  slope <- model$coefficients[2]

  ## get time column name
  time_column <- colnames(.data)[1]

  out <- .data %>%
    dplyr::mutate(work_rate = dplyr::case_when(
      protocol_phase == "baseline" ~ baseline_intensity,
      protocol_phase == "ramp" ~ baseline_intensity + slope * !!rlang::sym(time_column)
    ))

  out
}

#' Work rate for a step-incremental test
#'
#' This function produces the work rate throughout a step-incremental test given the protocol
#' This will retrieve both the 'original' work rates, and also will perform a 'linearization' of the steps.
#'
#' @param .data The data with recognized protocol phases
#' @param baseline_intensity The baseline intensity
#' @param step_start In case the step test started in a different work rate than baseline
#' @param step_increase The step in increase, in watts per step
#' @param step_length The length, in seconds, of each step
#'
#' @return a [tibble][tibble::tibble-package]
#' @keywords internal
work_rate_step <- function(
  .data,
  baseline_intensity,
  step_start,
  step_increase,
  step_length
) {
  ## check if forcats is installed and have a prompt to install it.
  rlang::check_installed("forcats")

  if(!is.null(step_start)) {
    begin_intensity <- step_start
  } else {
    begin_intensity <- baseline_intensity
  }

  ## this will make sure that the final intensity of each step
  ## corresponds to the step intensity
  if(begin_intensity == 0) {
    # linearization
    model <- lm(work_rate ~ time, data = data.frame(work_rate = c(begin_intensity, begin_intensity + step_increase), time = c(0, step_length)))
  } else {
    # linearization
    model <- lm(work_rate ~ time, data = data.frame(work_rate = c(begin_intensity - step_increase, begin_intensity), time = c(0, step_length)))
  }

  slope <- model$coefficients[2]

  ## get time column name
  time_column <- colnames(.data)[1]

  ## this will make sure that the final intensity of each step
  ## corresponds to the step intensity
  if(begin_intensity == 0) {
    data_linearized <- .data %>%
      dplyr::mutate(work_rate = dplyr::case_when(
        protocol_phase == "baseline" ~ baseline_intensity,
        protocol_phase == "step" ~ begin_intensity + slope * !!rlang::sym(time_column)
      ))
  } else {
    data_linearized <- .data %>%
      dplyr::mutate(work_rate = dplyr::case_when(
        protocol_phase == "baseline" ~ baseline_intensity,
        protocol_phase == "step" ~ begin_intensity - step_increase + slope * !!rlang::sym(time_column)
      ))
  }

  # steps
  data_steps <- data_linearized %>%
    dplyr::mutate(step = paste0("step_", (!!rlang::sym(time_column) %/% step_length + 1)),
                  step = forcats::as_factor(step))

  n_steps <- length(unique(data_steps$step))

  out <- data_steps %>%
    tidyr::nest(data = -step) %>%
    dplyr::mutate(step_work_rate = c(baseline_intensity, seq(begin_intensity, n_steps * step_increase + baseline_intensity, step_increase))) %>%
    tidyr::unnest(cols = data) %>%
    dplyr::relocate(step, .after = dplyr::last_col())

  out
}
