#' Normalize incremental test data
#'
#' Detect protocol phases (baseline, ramp, steps), normalize work rate, and
#' time-align baseline phase (baseline time becomes negative).
#'
#' @param .data Data retrieved from \code{read_data()}.
#' @param incremental_type The type of the incremental test performed. Either "ramp" or "step".
#' @param has_baseline A boolean to indicate whether the data contains a baseline phase. This is used for an incremental test only. Default to `TRUE`.
#' @param baseline_length The baseline length (in seconds) performed.
#' @param work_rate_magic A boolean indicating whether to perform the work rate calculations. When set to `TRUE`,
#' it will calculate the work rate throughout a ramp or step test. In the case of a step test, it will also
#' perform a linear transformation of the work rate.
#' If set to `TRUE`, the arguments below should be given. Default to `FALSE`.
#' @param baseline_intensity A numeric atomic vector indicating the work rate of the baseline. If the baseline was performed at rest, indicate `0`.
#' @param ramp_increase A numeric atomic vector indicating the ramp increase in watts per minute (W/min). For example, if the ramp
#' was `30 W/min`, then pass the number `30` to this argument.
#' @param step_start In case your baseline was performed at rest, you can set in this parameter at which intensity
#' the step test started.
#' @param step_increase A numeric atomic vector indicating the step increase, in watts. For example, if the step increase was
#' `25 W` at each step, then pass the number `25` to this argument.
#' @param step_length A numeric atomic vector indicating the length (in seconds) of each step in the step incremental test.
#' @param ... Additional arguments. Currently ignored.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' ## get file path from example data
#' path_example <- system.file("ramp_cosmed.xlsx", package = "whippr")
#'
#' ## read data from ramp test
#' df <- read_data(path = path_example, metabolic_cart = "cosmed")
#'
#' ## normalize incremental test data
#' ramp_normalized <- df %>%
#'  incremental_normalize(
#'    .data = .,
#'    incremental_type = "ramp",
#'    has_baseline = TRUE,
#'    baseline_length = 240,
#'    work_rate_magic = TRUE,
#'    baseline_intensity = 20,
#'    ramp_increase = 25
#'  )
#'
#' ## get file path from example data
#' path_example_step <- system.file("step_cortex.xlsx", package = "whippr")
#'
#' ## read data from step test
#' df_step <- read_data(path = path_example_step, metabolic_cart = "cortex")
#'
#' ## normalize incremental test data
#' step_normalized <- df_step %>%
#'  incremental_normalize(
#'    .data = .,
#'    incremental_type = "step",
#'    has_baseline = TRUE,
#'    baseline_length = 120,
#'    work_rate_magic = TRUE,
#'    baseline_intensity = 0,
#'    step_start = 50,
#'    step_increase = 25,
#'    step_length = 180
#'  )
incremental_normalize <- function(
  .data,
  incremental_type = c("ramp", "step"),
  has_baseline = TRUE,
  baseline_length = NULL,
  work_rate_magic = FALSE,
  baseline_intensity = NULL,
  ramp_increase = NULL,
  step_start = NULL,
  step_increase = NULL,
  step_length = NULL,
  ...
) {

  if(missing(.data))
    stop("No data, no fun. Please, pass the data retrieved from 'read_data()' to the function.", call. = FALSE)

  if(is.null(attributes(.data)$read_data))
    stop("It looks like you did not read your data with the `read_data()` function. Make sure you use it before continuing.", call. = FALSE)

  incremental_type <- match.arg(incremental_type)

  class(.data) <- incremental_type

  UseMethod("incremental_normalize", .data)
}

#' @export
incremental_normalize.ramp <- function(
  .data,
  incremental_type = c("ramp", "step"),
  has_baseline = TRUE,
  baseline_length = NULL,
  work_rate_magic = FALSE,
  baseline_intensity = NULL,
  ramp_increase = NULL,
  step_start = NULL,
  step_increase = NULL,
  step_length = NULL,
  ...
) {

  if(has_baseline & missing(baseline_length))
    stop("You indicated that your data contains a baseline phase, but you forgot to specify the length of the
         baseline. Please, indicate it in the 'baseline_length' argument.", call. = FALSE)

  # 1) time-align --------------------------------------------------------------
  if(has_baseline) {
    data_time_aligned <- .data %>%
      dplyr::mutate(dplyr::across(.cols = 1, .fns = ~ .x - baseline_length))
  } else {
    data_time_aligned <- .data
  }

  time_column <- attributes(.data)$time_column

  # 2) identify protocol phases ------------------------------------------------
  if(has_baseline) {
    out <- data_time_aligned %>%
      dplyr::mutate(protocol_phase = dplyr::case_when(
        !!rlang::sym(time_column) <= 0 ~ "baseline",
        TRUE ~ "ramp"
      ))
  } else {
    out <- data_time_aligned %>%
      dplyr::mutate(protocol_phase = "ramp")
  }

  ## work rate magic
  if(work_rate_magic & any(c(missing(baseline_intensity), missing(ramp_increase))))
    stop("For the work rate magic to work you need to specify the 'baseline_intensity' and 'ramp_increase' arguments.", call. = FALSE)

  if(work_rate_magic)
    out <- work_rate_ramp(.data = out, baseline_intensity = baseline_intensity, ramp_increase = ramp_increase)

  metadata <- attributes(.data)
  metadata$data_status <- "raw data - ramp normalized"
  metadata$test_type <- "incremental"
  metadata$incremental <- TRUE
  metadata$normalized <- TRUE
  metadata$incremental_type <- incremental_type
  metadata$has_baseline <- has_baseline
  metadata$baseline_length <- baseline_length
  metadata$baseline_intensity <- baseline_intensity
  metadata$ramp_increase <- ramp_increase

  out <- new_whippr_tibble(out, metadata)

  out
}

#' @export
incremental_normalize.step <- function(
  .data,
  incremental_type = c("ramp", "step"),
  has_baseline = TRUE,
  baseline_length = NULL,
  work_rate_magic = FALSE,
  baseline_intensity = NULL,
  ramp_increase = NULL,
  step_start = NULL,
  step_increase = NULL,
  step_length = NULL,
  ...
) {

  if(any(is.null(step_increase), is.null(step_length)))
    stop("You need to specify the `step_increase` and `step_length` arguments for the step test.", call. = FALSE)

  if(has_baseline & missing(baseline_length))
    stop("You indicated that your data contains a baseline phase, but you forgot to specify the length of the
         baseline. Please, indicate it in the 'baseline_length' argument.", call. = FALSE)

  # 1) time-align --------------------------------------------------------------
  if(has_baseline) {
    data_time_aligned <- .data %>%
      dplyr::mutate(dplyr::across(.cols = 1, .fns = ~ .x - baseline_length))
  } else {
    data_time_aligned <- .data
  }

  time_column <- attributes(.data)$time_column

  # 2) identify protocol phases ------------------------------------------------
  if(has_baseline) {
    out <- data_time_aligned %>%
      dplyr::mutate(protocol_phase = dplyr::case_when(
        !!rlang::sym(time_column) <= 0 ~ "baseline",
        TRUE ~ "step"
      ))
  } else {
    out <- data_time_aligned %>%
      dplyr::mutate(protocol_phase = "step")
  }

  ## work rate magic
  if(work_rate_magic & any(c(missing(baseline_intensity), missing(step_increase), missing(step_length))))
    stop("For the work rate magic to work you need to specify the 'baseline_intensity', 'step_increase', and 'step_length' arguments.", call. = FALSE)

  if(work_rate_magic)
    out <- work_rate_step(
      .data = out,
      baseline_intensity = baseline_intensity,
      step_start = step_start,
      step_increase = step_increase,
      step_length = step_length
    )

  metadata <- attributes(.data)
  metadata$data_status <- "raw data - step normalized"
  metadata$test_type <- "incremental"
  metadata$incremental <- TRUE
  metadata$normalized <- TRUE
  metadata$incremental_type <- incremental_type
  metadata$has_baseline <- has_baseline
  metadata$baseline_length <- baseline_length
  metadata$baseline_intensity <- baseline_intensity
  metadata$step_start <- step_start
  metadata$step_increase <- step_increase
  metadata$step_length <- step_length

  out <- new_whippr_tibble(out, metadata)

  out

}

#' Plot incremental test work rate
#'
#' Visualize what was done during the process of deriving the work rate from the incremental test protocol
#'
#' @param .data data retrieved from `incremental_normalize()`.
#'
#' @return a ggplot object
#' @export
plot_incremental <- function(.data) {

  if(is.null(attr(.data, "normalized")))
    stop("It looks like you did not normalized your incremental data yet with the `incremental_normalize()` function.
         Make sure you use it before continuing.", call. = FALSE)

  class(.data) <- attr(.data, "incremental_type")

  UseMethod("plot_incremental", .data)

}

#' @export
plot_incremental.ramp <- function(.data) {
  ## check if ggforce is installed
  if(length(find.package(package = "ggforce", quiet = TRUE)) == 0) {
    stop("You need to install the ggforce package to use this function.", call. = FALSE)
  }

  ## get time column
  time_column <- attr(.data, "time_column")

  df_labels <- .data %>%
    dplyr::group_by(protocol_phase) %>%
    dplyr::summarise(x = mean(t), y = mean(work_rate)) %>%
    dplyr::mutate(desc = glue::glue("{protocol_phase} period work rate"))

  out <- .data %>%
    ggplot2::ggplot(ggplot2::aes(!!rlang::sym(time_column), work_rate)) +
    ggplot2::geom_line() +
    ggforce::geom_mark_circle(
      data = df_labels,
      ggplot2::aes(x, y, label = protocol_phase, description = desc),
      expand = ggplot2::unit(2, "mm")
    ) +
    theme_whippr()

  out
}

#' @export
plot_incremental.step <- function(.data) {

  ## get time column
  time_column <- attr(.data, "time_column")

  df_labels <- .data %>%
    dplyr::group_by(step, step_work_rate) %>%
    dplyr::summarise(t_step = mean(!!rlang::sym(time_column))) %>%
    dplyr::mutate(
      label = stringr::str_extract(string = step, pattern = "\\d.*"),
      label = glue::glue("Step {label}")
    )

  df_seg <- .data %>%
    dplyr::filter(protocol_phase == "step") %>%
    dplyr::select(!!rlang::sym(time_column), work_rate) %>%
    dplyr::summarise(
      x = mean(!!rlang::sym(time_column)),
      y = mean(work_rate)
    ) %>%
    dplyr::mutate(
      label = "Continuous WR",
      desc = "Linearization of the steps"
    )

  df_seg_2 <- .data %>%
    dplyr::filter(protocol_phase == "step") %>%
    dplyr::select(t, step_work_rate, step) %>%
    dplyr::group_by(step) %>%
    dplyr::summarise(
      x = min(!!rlang::sym(time_column)),
      y = min(step_work_rate)
    ) %>%
    dplyr::slice(nrow(.) / 2) %>%
    dplyr::mutate(
      label = "Steps performed",
      desc = "Work rate of the given step"
    )

  p <- .data %>%
    ggplot2::ggplot(ggplot2::aes(!!rlang::sym(time_column), step_work_rate)) +
    ggplot2::geom_path(color = "black") +
    ggplot2::geom_path(ggplot2::aes(t, work_rate), color = "darkred", lty = "dashed") +
    ggplot2::geom_text(
      data = df_labels,
      ggplot2::aes(t_step, step_work_rate + 10, label = label),
      fontface = "bold"
    ) +
    ggforce::geom_mark_circle(
      data = df_seg,
      ggplot2::aes(x, y, label = label, description = desc),
      label.colour = "darkred",
      expand = ggplot2::unit(2, "mm"),
      label.buffer = ggplot2::unit(70, "mm")
    ) +
    ggforce::geom_mark_circle(
      data = df_seg_2,
      ggplot2::aes(x, y, label = label, description = desc),
      expand = ggplot2::unit(2, "mm"),
      label.buffer = ggplot2::unit(30, "mm")
    ) +
    ggplot2::labs(
      title = "Step incremental test",
      subtitle = "Description of the linearization performed on the work rate",
      x = "time (s)",
      y = "Step work rate (W)"
    ) +
    theme_whippr()

  p
}
