#' VO2max
#'
#' It performs the whole process of the VO2max data analysis, which includes:
#' data standardization and normalization according to incremental protocol (`incremental_normalize()`),
#' 'bad breaths' detection (`detect_outliers()`),
#' mean response time calculation (`incremental_mrt()`) (currently ignored),
#' and maximal values calculation (VO2, PO, HR, RER) (`perform_max()`).
#'
#' @param .data Data retrieved from `read_data()`.
#' @param vo2_column The name (quoted) of the column containing the absolute oxygen uptake (VO2) data. Default to `"VO2"`.
#' @param vo2_relative_column The name (quoted) of the column containing the relative to body weight oxygen uptake (VO2) data. Default to `NULL`.
#' @param heart_rate_column The name (quoted) of the column containing the heart rate (HR) data. Default to `NULL`. If `NULL`, this parameter will not be calculated.
#' @param rer_column The name (quoted) of the column containing the respiratory exchange ratio (RER) data. Default to `NULL`. If `NULL`, this parameter will not be calculated.
#' @param average_method The average method to be used for VO2max calculation. One of `bin` or `rolling`.
#' @param average_length The length, in seconds, of the average to be used. For example, if `average_method = bin`, and `average_length = 30`, it will perform a 30-s bin-average.
#' @param detect_outliers A boolean indicating whether to detect outliers. Default to `TRUE`.
#' @param mrt A boolean indicating whether to calculate the mean response time. To be implemented soon <-  currently ignored.
#' @param plot A boolean indicating whether to produce a plot with the summary results. Default to `TRUE`.
#' @param verbose A boolean indicating whether messages should be printed in the console. Default to `TRUE`.
#' @param ... Additional arguments passed onto `incremental_normalize()`, `detect_outliers()` if `detect_outliers = TRUE`, and  `incremental_mrt()` if `mrt = TRUE`.
#'
#' @return a [tibble][tibble::tibble-package] containing one row and the following columns:
#' \item{VO2max_absolute}{The absolute VO2max.}
#' \item{VO2max_relative}{The relative VO2max.}
#' \item{POpeak}{The peak power output.}
#' \item{HRmax}{The maximal heart rate.}
#' \item{RERmax}{The maximal RER.}
#' \item{plot}{The plot, if `plot = TRUE`.}
#'
#' @details
#' TODO
#'
#' @export
#'
#' @examples
#' \dontrun{
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
#' ## detect outliers
#' data_ramp_outliers <- detect_outliers(
#'  .data = ramp_normalized,
#'  test_type = "incremental",
#'  vo2_column = "VO2",
#'  cleaning_level = 0.95,
#'  method_incremental = "linear",
#'  verbose = TRUE
#' )
#'
#' ## analyze VO2max
#' perform_max(
#'  .data = data_ramp_outliers,
#'  vo2_column = "VO2",
#'  vo2_relative_column = "VO2/Kg",
#'  heart_rate_column = "HR",
#'  rer_column = "R",
#'  average_method = "bin",
#'  average_length = 30,
#'  plot = TRUE,
#'  verbose = FALSE
#' )
#' }
#'
vo2_max <- function(
  .data,
  vo2_column = "VO2",
  vo2_relative_column = NULL,
  heart_rate_column = NULL,
  rer_column = NULL,
  detect_outliers = TRUE,
  average_method = c("bin", "rolling"),
  average_length = 30,
  mrt,
  plot = TRUE,
  verbose = TRUE,
  ...
) {

  if(!attr(.data, "read_data"))
    stop("You need to read your data with `read_data()` first.", call. = FALSE)

  if(verbose)
    cli::cli_rule(center = cli::col_red(" * V\u0307O\u2082 max analysis * "))

  if(verbose)
    usethis::ui_done("Normalizing incremental data...")

  ## normalize incremental test
  data_normalized <- incremental_normalize(.data = .data, ...)

  ## detect outliers
  if(detect_outliers) {

    data_normalized <- data_normalized %>%
      detect_outliers(
        .data = .,
        verbose = verbose,
        ...
      )
  }

  ## perform VO2max
  out <- perform_max(
    .data = data_normalized,
    vo2_column = vo2_column,
    vo2_relative_column = vo2_relative_column,
    heart_rate_column = heart_rate_column,
    rer_column = rer_column,
    average_method = average_method,
    average_length = average_length,
    plot = plot
  )

  out
}

#' Perform VO2max calculation
#'
#' It performs the calculation of VO2max, HRmax, and maximal RER. Additionally, it detects whether a plateau can be identified from your data.
#'
#' @param .data The data retrieved either from `incremental_normalize()` or `detect_outliers()`.
#' @param vo2_column The name (quoted) of the column containing the absolute oxygen uptake (VO2) data. Default to `"VO2"`.
#' @param vo2_relative_column The name (quoted) of the column containing the relative to body weight oxygen uptake (VO2) data. Default to `NULL`.
#' @param heart_rate_column The name (quoted) of the column containing the heart rate (HR) data. Default to `NULL`. If `NULL`, this parameter will not be calculated.
#' @param rer_column The name (quoted) of the column containing the respiratory exchange ratio (RER) data. Default to `NULL`. If `NULL`, this parameter will not be calculated.
#' @param average_method The average method to be used for VO2max calculation. One of `bin` or `rolling`.
#' @param average_length The length, in seconds, of the average to be used. For example, if `average_method = bin`, and `average_length = 30`, it will perform a 30-s bin-average.
#' @param plot A boolean indicating whether to produce a plot with the summary results. Default to `TRUE`.
#' @param verbose A boolean indicating whether messages should be printed in the console. Default to `TRUE`.
#'
#' @return a tibble
#' @export
#'
#' @examples
#' \dontrun{
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
#' ## detect outliers
#' data_ramp_outliers <- detect_outliers(
#'  .data = ramp_normalized,
#'  test_type = "incremental",
#'  vo2_column = "VO2",
#'  cleaning_level = 0.95,
#'  method_incremental = "linear",
#'  verbose = TRUE
#' )
#'
#' ## analyze VO2max
#' perform_max(
#'  .data = data_ramp_outliers,
#'  vo2_column = "VO2",
#'  vo2_relative_column = "VO2/Kg",
#'  heart_rate_column = "HR",
#'  rer_column = "R",
#'  average_method = "bin",
#'  average_length = 30,
#'  plot = TRUE,
#'  verbose = FALSE
#' )
#' }
perform_max <- function(
  .data,
  vo2_column = "VO2",
  vo2_relative_column = NULL,
  heart_rate_column = NULL,
  rer_column = NULL,
  average_method = c("bin", "rolling"),
  average_length = 30,
  plot = TRUE,
  verbose = TRUE
) {

  ## check args
  average_method <- match.arg(average_method)

  if(average_length < 0)
    stop("You can't choose a negative number for averaging the data.", call. = FALSE)

  ## check if ramp was normalized
  if(is.null(attr(.data, "normalized")))
    stop("It looks like you did not normalize your incremental data. Did you forget to use `incremental_normalize()`?", call. = FALSE)

  ## check column names
  if(!is.null(vo2_column))
    if(!vo2_column %in% colnames(.data))
      stop(glue::glue("It looks like the column {vo2_column} does not exist."), call. = FALSE)

  if(!is.null(vo2_relative_column))
    if(!vo2_relative_column %in% colnames(.data))
      stop(glue::glue("It looks like the column {vo2_relative_column} does not exist."), call. = FALSE)

  if(!is.null(heart_rate_column))
    if(!heart_rate_column %in% colnames(.data))
      stop(glue::glue("It looks like the column {heart_rate_column} does not exist."), call. = FALSE)

  if(!is.null(rer_column))
    if(!rer_column %in% colnames(.data))
      stop(glue::glue("It looks like the column {rer_column} does not exist."), call. = FALSE)

  data_normalized <- .data

  ## check if outliers were identified
  ## if not, throw a warning
  if(is.null(attr(data_normalized, "outliers_detected")))
    warning("You did not identify any outliers prior to using this function. You should consider using `detect_outliers()` before.", call. = FALSE)

  ## make sure no outliers exist
  if(attr(data_normalized, "outliers_detected")) {

    if(verbose)
      usethis::ui_done("Filtering out outliers...")

    data_normalized_filtered <- dplyr::filter(data_normalized, outlier == "no")
  }

  if(verbose) {
    usethis::ui_done("Interpolating from breath-by-breath into second-by-second...")
    usethis::ui_done("Performing averages...")
  }

  ## interpolate and average data
  data_averaged <- data_normalized_filtered %>%
    ## interpolate data from breath-by-breath to second-by-second
    interpolate() %>%
    ## perform the chosen average method
    perform_average(type = "bin", bins = 30)

  ## plateau detection
  ## TODO

  ## results
  ### a few notes:
  ### VO2 and RER are analyzed through the averaged data
  ### HR and PO are analyzed from the breath-by-breath data
  out <- dplyr::tibble(
    VO2max_absolute = max(data_averaged[[vo2_column]]),
    VO2max_relative = ifelse(is.null(vo2_relative_column), NA, max(data_averaged[[vo2_relative_column]])),
    POpeak = as.integer(max(data_normalized[["work_rate"]])),
    HRmax = ifelse(is.null(heart_rate_column), NA, max(data_normalized[[heart_rate_column]])),
    RERmax = ifelse(is.null(rer_column), NA, max(data_averaged[[rer_column]]))
  )

  if(plot) {
    ## check if ggforce and ggtext are installed
    if(length(find.package(package = c("ggforce", "ggtext"), quiet = TRUE)) == 0) {
      stop("You need to install the ggforce and ggtext packages to use this function.", call. = FALSE)
    }
    label_graph <- out %>%
      tidyr::pivot_longer(cols = dplyr::everything()) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(
        value = dplyr::case_when(
          name == "RERmax" ~ round(value, 2),
          name == "VO2max_relative" ~ round(value, 1),
          TRUE ~ round(value, )
        )
      ) %>%
      dplyr::mutate(
        units = dplyr::case_when(
          name == "VO2max_absolute" ~ "mL/min",
          name == "VO2max_relative" ~ "mL/kg/min",
          name == "POpeak" ~ "W",
          name == "HRmax" ~ "bpm",
          name == "RERmax" ~ ""
        )
      ) %>%
      dplyr::summarise(label = paste(name, "=", value, units, collapse = "\n")) %>%
      dplyr::pull()

    ## plot
    p <- data_averaged %>%
      dplyr::filter(t > 0) %>%
      ggplot2::ggplot(ggplot2::aes(work_rate, VO2)) +
      ggplot2::geom_point(shape = 21, size = 4, color = "black", fill = "white") +
      ggforce::geom_mark_rect(ggplot2::aes(filter = work_rate > out$POpeak * 0.9, label = "Maximal values", description = label_graph), label.minwidth = ggplot2::unit(100, "mm")) +
      ggplot2::labs(
        title = "Incremental test",
        subtitle = "Summary results",
        x = "Work Rate (W)",
        y = "V&#775;O<sub>2</sub>"
      ) +
      theme_whippr() +
      ggplot2::theme(axis.title.y = ggtext::element_markdown())

    out <- out %>%
      dplyr::mutate(plot = list(p))
  }

  out
}
