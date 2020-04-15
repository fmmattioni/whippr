#' Normalize first breath
#'
#' This is needed specially when the data gets filtered. For example, if the data file does not only contain
#' the baseline and transitions performed, we will have to normalize the time column.
#' This function will make sure that in case the first breath does not start at zero, it will create a zero data point,
#' duplicating the first breath. This will make sure the data does not get shifted (misalignment).
#'
#' @param .data Breath-by-breath data.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
normalize_first_breath <- function(.data) {
  first_breath <- .data %>%
    dplyr::select(1) %>%
    dplyr::slice(1) %>%
    dplyr::pull() %>%
    as.integer()

  if(first_breath %% 10 != 0) {
    row_to_add <- .data[1, ] %>%
      dplyr::mutate_at(1, function(x) x = first_breath - first_breath %% 10)

    out <- .data %>%
      dplyr::bind_rows(row_to_add, .)
  } else {
    out <- .data
  }

  out
}

#' Normalize transitions
#'
#' Recognizes and normalizes the time column of each transition. It will also label the transitions into: 'baseline' or 'transition'.
#'
#' @param .data Breath-by-breath data.
#' @param protocol_n_transitions Number of transitions performed.
#' @param protocol_baseline_length The length of the baseline (in seconds).
#' @param protocol_transition_length The length of the transition (in seconds).
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
normalize_transitions <- function(
  .data,
  protocol_n_transitions,
  protocol_baseline_length,
  protocol_transition_length
) {
  ## first step is to get the time that each transition ends (in seconds),
  ## and then apply the label to each transition
  info_transition <- dplyr::tibble(
    n_transition = seq(1, protocol_n_transitions, 1)
  ) %>%
    dplyr::mutate(
      end_transition = (protocol_baseline_length + protocol_transition_length) * n_transition,
      label = paste("transition", n_transition, sep = " ")
    )

  ## get time column name
  time_column <- colnames(.data)[1]

  ## now we normalize the first breath for the whole data
  ## for more information see ?normalize_first_breath()
  .data %>%
    normalize_first_breath() %>%
    ## normalize the whole time column in case the test didn't start at zero
    dplyr::mutate_at(1, function(x) x = x - min(x)) %>%
    ## label transitions
    dplyr::mutate(transition = cut(!!rlang::sym(time_column), c(0, info_transition$end_transition), labels = info_transition$label, include.lowest = TRUE)) %>%
    ## now we normalize the first breath for each transition
    ## for more information see ?normalize_first_breath()
    tidyr::nest_legacy(-transition) %>%
    dplyr::mutate(data = purrr::map(.x = data, .f = normalize_first_breath)) %>%
    tidyr::unnest_legacy() %>%
    dplyr::select(2:ncol(.), transition) %>%
    dplyr::group_by(transition) %>%
    dplyr::mutate_at(1, function(x) x = x - min(x)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(phase = ifelse(.[[1]] <= protocol_baseline_length, "baseline", "transition"))
}

#' Normalize time column
#'
#' Normalizes the the time column such that the baseline phase has negative time values. Point zero will then represent the start of the transition phase.
#'
#' @param .data Breath-by-breath data.
#' @param protocol_baseline_length The length of the baseline (in seconds).
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
normalize_time <- function(.data, protocol_baseline_length) {
  out <- .data %>%
    dplyr::mutate_at(1, function(x) x = x - protocol_baseline_length)

  out
}
