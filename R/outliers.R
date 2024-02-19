#' Detect outliers
#'
#' It detects outliers based on prediction bands for the given level of confidence provided.
#'
#' @param .data Data retrieved from `read_data()` for a **kinetics** test, or
#' the data retrieved from `incremental_normalize()` for a **incremental** test.
#' @param test_type The test to be analyzed. Either 'incremental' or 'kinetics'.
#' @param vo2_column The name (quoted) of the column containing the absolute oxygen uptake (VO2) data. Default to `VO2`.
#' @param cleaning_level A numeric scalar between 0 and 1 giving the confidence level for the intervals to be calculated. Default to `0.95`.
#' @param cleaning_baseline_fit For **kinetics** test only. A vector of the same length as the number in `protocol_n_transitions`, indicating what kind of fit to perform for each baseline. Vector accepts characters either 'linear' or 'exponential'.
#' @param protocol_n_transitions For **kinetics** test only. Number of transitions performed.
#' @param protocol_baseline_length For **kinetics** test only. The length of the baseline (in seconds).
#' @param protocol_transition_length For **kinetics** test only. The length of the transition (in seconds).
#' @param method_incremental The method to be used in detecting outliers from the
#' incremental test. Either 'linear' or 'anomaly'. See `Details`.
#' @param verbose A boolean indicating whether messages should be printed in the console. Default to `TRUE`.
#' @param ... Additional arguments. Currently ignored.
#'
#' @details
#' TODO
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' \dontrun{
#' ## get file path from example data
#' path_example <- system.file("example_cosmed.xlsx", package = "whippr")
#'
#' ## read data
#' df <- read_data(path = path_example, metabolic_cart = "cosmed")
#'
#' ## detect outliers
#' data_outliers <- detect_outliers(
#'   .data = df,
#'   test_type = "kinetics",
#'   vo2_column = "VO2",
#'   cleaning_level = 0.95,
#'   cleaning_baseline_fit = c("linear", "exponential", "exponential"),
#'   protocol_n_transitions = 3,
#'   protocol_baseline_length = 360,
#'   protocol_transition_length = 360,
#'   verbose = TRUE
#'  )
#'
#' ## get file path from example data
#' path_example_ramp <- system.file("ramp_cosmed.xlsx", package = "whippr")
#'
#' ## read data from ramp test
#' df_ramp <- read_data(path = path_example_ramp, metabolic_cart = "cosmed")
#'
#' ## normalize incremental test data
#' ramp_normalized <- df_ramp %>%
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
#' ## detect ramp outliers
#' data_ramp_outliers <- detect_outliers(
#'   .data = ramp_normalized,
#'   test_type = "incremental",
#'   vo2_column = "VO2",
#'   cleaning_level = 0.95,
#'   method_incremental = "linear",
#'   verbose = TRUE
#'  )
#' }
detect_outliers <- function(
  .data,
  test_type = c("incremental", "kinetics"),
  vo2_column = "VO2",
  cleaning_level = 0.95,
  cleaning_baseline_fit,
  protocol_n_transitions,
  protocol_baseline_length,
  protocol_transition_length,
  method_incremental = c("linear", "anomaly"),
  verbose = TRUE,
  ...
) {

  if(missing(.data))
    stop("No data, no fun. Please, pass the data retrieved from 'read_data()' to the function.", call. = FALSE)

  if(is.null(attributes(.data)$read_data))
    stop("It looks like you did not read your data with the `read_data()` function. Make sure you use it before continuing.", call. = FALSE)

  if(missing(test_type))
    stop("You must specify the type of test these data are related to. See ?detect_outliers for more information.", call. = FALSE)

  if(!vo2_column %in% colnames(.data))
    stop("It looks like the name of the VO2 column you chose does not exist.", call. = FALSE)

  class(.data) <- test_type

  UseMethod("detect_outliers", .data)
}

#' @export
detect_outliers.kinetics <- function(
  .data,
  test_type = c("incremental", "kinetics"),
  vo2_column = "VO2",
  cleaning_level = 0.95,
  cleaning_baseline_fit,
  protocol_n_transitions,
  protocol_baseline_length,
  protocol_transition_length,
  method_incremental = c("linear", "anomaly"),
  verbose = TRUE,
  ...
) {

  if(missing(protocol_n_transitions))
    stop("You must specify the number of transitions perfomed. See ?detect_outliers for more information.", call. = FALSE)

  if(missing(protocol_baseline_length))
    stop("You must specify the length of the baseline phase. See ?detect_outliers for more information.", call. = FALSE)

  if(missing(protocol_transition_length))
    stop("You must specify the length of the transition phase. See ?detect_outliers for more information.", call. = FALSE)

  if(missing(cleaning_baseline_fit))
    stop("You must specify the type of fit to be performed in each baseline phase. See ?detect_outliers for more information.", call. = FALSE)

  if(length(cleaning_baseline_fit) != protocol_n_transitions)
    stop(
      glue::glue(
        "You must indicate the baseline fit for each baseline.
         You indicated {protocol_n_transitions} were performed. You need to pass to the 'cleaning_baseline_fit' argument a vector of length {protocol_n_transitions}.
         For example, if 3 transitions were performed, you would write cleaning_baseline_fit = c('linear', 'exponential', 'exponential')."
      ),
      call. = FALSE
    )

  if(verbose)
    cli::cli_alert_success("Detecting outliers")

  ## check if data passed to the function contains more data points than it should
  max_length <- protocol_n_transitions * (protocol_baseline_length + protocol_transition_length)

  if(max(.data[[1]]) > max_length)
    stop(
      "It looks like you have more data points than indicated.
       That means that perhaps you need to use 'dplyr::filter()' to make sure the data passed to the function only contains what you want to analyse.",
      call. = FALSE
    )

  ## get time column from attributes
  time_column <- attributes(.data)$time_column

  out <- .data %>%
    normalize_transitions(
      .data = .,
      protocol_n_transitions = protocol_n_transitions,
      protocol_baseline_length = protocol_baseline_length,
      protocol_transition_length = protocol_transition_length
    ) %>%
    tidyr::nest(data = -transition) %>%
    dplyr::mutate(
      cleaning_baseline_fit = cleaning_baseline_fit,
      bands_data = purrr::map2(
        .x = data,
        .y = cleaning_baseline_fit,
        .f = ~ predict_bands(
          .data = .x,
          cleaning_baseline_fit = .y,
          cleaning_level = cleaning_level,
          time_column = time_column,
          vo2_column = vo2_column)
      )
    ) %>%
    tidyr::unnest(cols = c(data, bands_data)) %>%
    dplyr::select(2:ncol(.), transition)

  if(verbose) {
    verbose_vector <- out %>%
      dplyr::count(transition, outlier) %>%
      dplyr::filter(outlier == "yes")

    if(nrow(verbose_vector) == 0) {
      cli::cli_alert_success("No outliers found.")
    } else {
       list <- dplyr::pull(verbose_vector, n, name = transition)
       purrr::iwalk(list, function(n, transition) {
         # display n outliers found in transition x
         # Using cli pluralization (means the s won't show if n_outliers = 1)
         cli::cli_ul("{n} outlier{?s} found in {transition}")
       })
    }
  }

  metadata <- attributes(.data)
  metadata$vo2_column <- vo2_column
  metadata$data_status <- "raw data - outliers detected"
  metadata$test_type <- "kinetics"

  out <- new_whippr_tibble(out, metadata)

  out

}

#' @export
detect_outliers.incremental <- function(
  .data,
  test_type = c("incremental", "kinetics"),
  vo2_column = "VO2",
  cleaning_level = 0.95,
  cleaning_baseline_fit,
  protocol_n_transitions,
  protocol_baseline_length,
  protocol_transition_length,
  method_incremental = c("linear", "anomaly"),
  verbose = TRUE,
  ...
) {

  if(is.null(attributes(.data)$normalized))
    stop("It looks like the data passed to this function did not come from the `incremental_normalize()` function.
         Please, make sure you use this function before.", call. = FALSE)

  if(missing(method_incremental))
    stop("You must specify the 'method_incremental' argument. See ?detect_outliers for more details.", call. = FALSE)

  method_incremental <- match.arg(method_incremental)
  time_column <- attributes(.data)$time_column

  if(verbose)
    cli::cli_alert_success("Detecting outliers")

  out <- switch (method_incremental,
                 "linear" = outliers_linear(
                   .data = .data,
                   time_column = time_column,
                   vo2_column = vo2_column,
                   cleaning_level = cleaning_level
                 ),
                 "anomaly" = outliers_anomaly(
                   .data = .data,
                   time_column = time_column,
                   vo2_column = vo2_column,
                   cleaning_level = cleaning_level
                 )
  )

  if(verbose) {
    verbose_vector <- out %>%
      dplyr::group_by(protocol_phase) %>%
      dplyr::count(outlier) %>%
      dplyr::ungroup() %>%
      dplyr::filter(outlier == "yes")

    if(nrow(verbose_vector) == 0) {
      cli::cli_alert_success("No outliers found.")
    } else {
      verbose_vector <- verbose_vector %>%
        dplyr::mutate(verbose = purrr::map2_chr(.x = n, .y = protocol_phase, .f = ~ glue::glue("{.x} outlier(s) found in {.y}"))) %>%
        dplyr::pull(verbose)

      purrr::walk(.x = verbose_vector, .f = cli::cli_ul)
    }
  }

  metadata <- attributes(.data)
  metadata$vo2_column <- vo2_column
  metadata$data_status <- "raw data - outliers detected"
  metadata$test_type <- "incremental"
  metadata$outliers_detected <- TRUE

  out <- new_whippr_tibble(out, metadata)

  out
}

#' Plot outliers
#'
#' @param .data The data retrieved from `detect_outliers()`.
#'
#' @return a patchwork object
#' @export
plot_outliers <- function(.data) {

  if(missing(.data))
    stop("No data, no fun. Please, pass the data retrieved from 'detect_outliers()' to the function.", call. = FALSE)

  if(!"outlier" %in% colnames(.data))
    stop("Are you sure you passed the data retrieved from 'detect_outliers()'?", call. = FALSE)

  test_type <- attributes(.data)$test_type

  class(.data) <- test_type

  UseMethod("plot_outliers", .data)
}

#' @export
plot_outliers.kinetics <- function(.data) {

  n_transitions <- .data %>%
    dplyr::pull(transition) %>%
    unique() %>%
    length()

  plots_ready <- .data %>%
    dplyr::pull(transition) %>%
    unique() %>%
    purrr::map(~{

      p1 <- .data %>%
        dplyr::filter(transition == .x) %>%
        ggplot2::ggplot() +
        ggplot2::geom_point(ggplot2::aes(x, y, fill = outlier), shape = 21, size = 4, color = "black") +
        ggplot2::geom_line(ggplot2::aes(x, .fitted), color = "red") +
        ggplot2::geom_ribbon(ggplot2::aes(x = x, ymin = lwr_pred, ymax = upr_pred), fill = "red", alpha = 0.1) +
        ggplot2::scale_fill_manual(values = c("white", "red")) +
        theme_whippr() +
        ggplot2::facet_wrap(~ transition)

      p2 <- .data %>%
        dplyr::filter(transition == .x) %>%
        ggplot2::ggplot(ggplot2::aes(x, .resid)) +
        ggplot2::geom_line(alpha = 0.4) +
        theme_whippr()

      patchwork::wrap_plots(p1, p2, ncol = 1, heights = c(8, 1))
    })

  if(n_transitions %in% c(1, 2)) {
    out <- patchwork::wrap_plots(plots_ready, nrow = 1, guides = "collect")
  } else {
    out <- patchwork::wrap_plots(plots_ready, nrow = 2, guides = "collect")
  }

  out

}

#' @export
plot_outliers.incremental <- function(.data) {
  out <- .data %>%
    ggplot2::ggplot(ggplot2::aes(x, y)) +
    ggplot2::geom_point(ggplot2::aes(fill = outlier), shape = 21, size = 4, color = "black") +
    ggplot2::geom_ribbon(ggplot2::aes(x = x, ymin = lwr_pred, ymax = upr_pred), fill = "red", alpha = 0.1) +
    ggplot2::scale_fill_manual(values = c("white", "red")) +
    ggplot2::labs(
      x = "time (s)",
      y = "VO2",
      title = "Detected outliers",
      subtitle = "In the incremental test"
    ) +
    theme_whippr()

  out
}
