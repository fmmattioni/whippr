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

  ## check if data passed to the function contains more data points than it should
  max_length <- protocol_n_transitions * (protocol_baseline_length + protocol_transition_length)

  if(max(.data[[1]]) > max_length)
    stop(
      "It looks like you have more data points than indicated.
       That means that perhaps you need to use 'dplyr::filter()' to make sure the data passed to the function only contains what you want to analyse.",
      call. = FALSE
    )

  ## get time column from attributes
  time_column <- "t"

  out <- .data %>%
    normalize_transitions(
      .data = .,
      protocol_n_transitions = protocol_n_transitions,
      protocol_baseline_length = protocol_baseline_length,
      protocol_transition_length = protocol_transition_length
    ) %>%
    tidyr::nest(data = -transition) %>%
    dplyr::mutate(
      cleaning_baseline_fit = cleaning_baseline_fit
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      bands_data = list(
        predict_bands(
          .data = data,
          cleaning_baseline_fit = cleaning_baseline_fit,
          cleaning_level = cleaning_level,
          time_column = time_column,
          vo2_column = vo2_column
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(cols = c(data, bands_data)) %>%
    dplyr::select(2:ncol(.), transition)

  out
}
