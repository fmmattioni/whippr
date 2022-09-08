#' Read data from metabolic cart
#'
#' It reads the raw data exported from the metabolic cart.
#'
#' @param path Path to read the file from.
#' @param metabolic_cart Metabolic cart that was used for data collection. Currently, 'cosmed', 'cortex', 'nspire', 'parvo', 'geratherm', and 'cardiocoach' are supported. Additionaly, there is an option called 'custom' that supports files that do not have a metabolic cart-specific format.
#' @param time_column The name (quoted) of the column containing the time. Depending on the language of your system, this column might not be "t". Therefore, you may specify it here.  Default to "t".
#' @param work_rate_column Default is `NULL`. In case your work rate column is coerced as a character column
#' you can define here the name of this column in your data file. This happens because at the very beginning of the test
#' the system may input a character like "-" to indicate no work rate. Therefore this is not going to get recognized as a numeric column.
#' If your work rate column is called `WR`, for example, just pass `"WR"` to this argument.
#'
#' @return a [tibble][tibble::tibble-package]
#' @importFrom rlang :=
#' @export
read_data <- function(
  path,
  metabolic_cart = c("cosmed", "cortex", "nspire", "parvo", "geratherm", "cardiocoach", "custom"),
  time_column = "t",
  work_rate_column = NULL
) {
  if(missing(metabolic_cart))
    stop("You must specify the metabolic cart.", call. = FALSE)

  metabolic_cart <- match.arg(metabolic_cart)

  class(path) <- metabolic_cart

  UseMethod("read_data", path)
}

#' @export
read_data.cosmed <- function(
  path,
  metabolic_cart = c("cosmed", "cortex", "nspire", "parvo", "geratherm", "cardiocoach", "custom"),
  time_column = "t",
  work_rate_column = NULL
) {
  data_raw <- suppressMessages(readxl::read_excel(path = path))

  ## find column that starts the data (time column will always be the first one)
  start_col <- target_cosmed(data_raw, time_column)

  if(purrr::is_empty(start_col))
    stop("It looks like the name of the time column you chose does not exist.", call. = FALSE)

  ## retrieve column names
  names_file <- data_raw[start_col:ncol(data_raw)] %>%
    names()

  ## this is necessary for defining the column types when reading the data
  ## depending on the COSMED version, the time column will be handled differently
  ## it will first try to parse is as a date, if it doesn't work then it will parse as numeric
  num_cols_pre <- start_col - 1
  num_cols_post <- ncol(data_raw) - start_col

  ## read data again, select only columns with data and apply column names
  data_raw2 <- suppressWarnings(suppressMessages(readxl::read_excel(
    path = path,
    skip = 1,
    col_types = c(rep("guess", num_cols_pre), "date", rep("guess", num_cols_post))
  ))) %>%
    dplyr::select(start_col:ncol(.)) %>%
    dplyr::rename_all(~ names_file) %>%
    remove_empty(which = "rows")

  ## if date parsing didnt't work, then let the read_excel function guess the type
  if(all(is.na(data_raw2[[1]]))) {
    ## read data again, select only columns with data and apply column names
    data_raw2 <- suppressMessages(readxl::read_excel(path = path, skip = 1)) %>%
      dplyr::select(start_col:ncol(.)) %>%
      dplyr::rename_all(~ names_file) %>%
      remove_empty(which = "rows")

    ## this will make sure that different versions of the cosmed will work with this function
    ## newer versions will display differnt time formats (00:00 instead of 00:00:00)
    data_raw2 <- data_raw2 %>%
      dplyr::rowwise() %>%
      dplyr::mutate_at(1, function(x) ifelse(nchar(x) == 5, paste0("00:", x), x)) %>%
      dplyr::ungroup()

    out <- data_raw2 %>%
      dplyr::mutate_at(1, function(x) stringr::str_replace_all(x, ",", ".") %>%
                         lubridate::hms(.) %>%
                         lubridate::period_to_seconds(.))
  } else {
    out <- data_raw2 %>%
      dplyr::mutate_at(1, function(x) (lubridate::hour(x) * 3600) + (lubridate::minute(x) * 60) + lubridate::second(x))
  }

  ## the following will try to coerce the work rate column to numeric
  ## and it renames the given work rate column into 'work_rate'
  if(!is.null(work_rate_column)) {
    if(!work_rate_column %in% colnames(out)) {
      stop("It looks like the work rate column you chose does not exist.", call. = FALSE)
    } else {
      out <- dplyr::mutate(out, !!work_rate_column := as.numeric(!!rlang::sym(work_rate_column)))
      out <- dplyr::rename(out, work_rate = !!rlang::sym(work_rate_column))
    }
  }

  metadata <- NULL
  metadata$read_data <- TRUE
  metadata$metabolic_cart <- "COSMED"
  metadata$data_status <- "raw data"
  metadata$time_column <- time_column

  out <- new_whippr_tibble(out, metadata)

  out
}

#' @export
read_data.cortex <- function(
  path,
  metabolic_cart = c("cosmed", "cortex", "nspire", "parvo", "geratherm", "cardiocoach", "custom"),
  time_column = "t",
  work_rate_column = NULL
) {
  data_raw <- suppressMessages(readxl::read_excel(path = path))

  ## find column that starts the data (time column will always be the first one)
  cells_cortex <- target_cortex(data_raw, time_column)

  if(purrr::is_empty(cells_cortex))
    stop("It looks like the name of the time column you chose does not exist.", call. = FALSE)

  ## retrieve column names
  names_file <- data_raw[cells_cortex[1],] %>%
    unlist(use.names = FALSE)

  ## read data again specifying starting row
  data_raw2 <- suppressMessages(readxl::read_excel(path = path, skip = cells_cortex[1] + 3, col_names = FALSE)) %>%
    dplyr::rename_all(~ names_file) %>%
    dplyr::mutate_if(is.character, ~ stringr::str_replace_all(., ",", "."))

  ## if time column was parsed as a date
  if(all(class(data_raw2[[1]]) %in% c("POSIXct", "POSIXt"))) {
    data_raw2 <- data_raw2 %>%
      dplyr::mutate_at(1, function(x) (lubridate::hour(x) * 3600) + (lubridate::minute(x) * 60) + lubridate::second(x))
  } else {
    ## if time column was parsed as numeric
    if(any(stringr::str_detect(data_raw2[[1]], ":"))){
      data_raw2 <- data_raw2 %>%
        dplyr::mutate_at(1, function(x) lubridate::hms(x) %>%
                           lubridate::period_to_seconds(.))
    } else{
      data_raw2 <- data_raw2 %>%
        dplyr::mutate_at(1, function(x) as.numeric(x)) %>%
        dplyr::mutate_at(1, function(x) x * 60 * 60 * 24)
    }
  }

  out <- data_raw2 %>%
    remove_empty(which = "cols")

  ## the following will try to coerce the work rate column to numeric
  ## and it renames the given work rate column into 'work_rate'
  if(!is.null(work_rate_column)) {
    if(!work_rate_column %in% colnames(out)) {
      stop("It looks like the work rate column you chose does not exist.", call. = FALSE)
    } else {
      out <- dplyr::mutate(out, !!work_rate_column := as.numeric(!!rlang::sym(work_rate_column)))
      out <- dplyr::rename(out, work_rate = !!rlang::sym(work_rate_column))
    }
  }

  metadata <- NULL
  metadata$read_data <- TRUE
  metadata$metabolic_cart <- "CORTEX"
  metadata$data_status <- "raw data"
  metadata$time_column <- time_column

  out <- new_whippr_tibble(out, metadata)

  out
}

#' @export
read_data.nspire <- function(
  path,
  metabolic_cart = c("cosmed", "cortex", "nspire", "parvo", "geratherm", "cardiocoach", "custom"),
  time_column = "t",
  work_rate_column = NULL
) {
  data_raw <- suppressMessages(readxl::read_excel(path = path))

  ## find column that starts the data (time column will always be the first one)
  start_col <- target_nspire(data_raw, time_column)

  if(purrr::is_empty(start_col))
    stop("It looks like the name of the time column you chose does not exist.", call. = FALSE)

  ## retrieve column names
  names_file <- data_raw[start_col:ncol(data_raw)] %>%
    names()

  out <- suppressWarnings(suppressMessages(readxl::read_excel(
    path = path,
    skip = 1))) %>%
    dplyr::select(start_col:ncol(.)) %>%
    dplyr::rename_all(~ names_file) %>%
    remove_empty(which = "rows")

  ## the following will try to coerce the work rate column to numeric
  ## and it renames the given work rate column into 'work_rate'
  if(!is.null(work_rate_column)) {
    if(!work_rate_column %in% colnames(out)) {
      stop("It looks like the work rate column you chose does not exist.", call. = FALSE)
    } else {
      out <- dplyr::mutate(out, !!work_rate_column := as.numeric(!!rlang::sym(work_rate_column)))
      out <- dplyr::rename(out, work_rate = !!rlang::sym(work_rate_column))
    }
  }

  metadata <- NULL
  metadata$read_data <- TRUE
  metadata$metabolic_cart <- "NSpire"
  metadata$data_status <- "raw data"
  metadata$time_column <- time_column

  out <- new_whippr_tibble(out, metadata)

  out
}

#' @export
read_data.parvo <- function(
  path,
  metabolic_cart = c("cosmed", "cortex", "nspire", "parvo", "geratherm", "cardiocoach", "custom"),
  time_column = "t",
  work_rate_column = NULL
) {
  data_raw <- suppressMessages(readxl::read_excel(path = path))

  ## find column that starts the data (time column will always be the first one)
  cells_parvo <- target_parvo(data_raw, time_column)

  if(purrr::is_empty(cells_parvo))
    stop("It looks like the name of the time column you chose does not exist.", call. = FALSE)

  ## retrieve column names
  names_file <- data_raw[cells_parvo[1]:(cells_parvo[1] + 1),] %>%
    unlist(use.names = FALSE) %>%
    matrix(., nrow = length(.) / 2, byrow = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(V1 = ifelse(V2 %in% c(NA, "STPD", "BTPS"), V1, paste0(V1, V2))) %>%
    .$V1

  ## read data again specifying starting row
  out <- suppressMessages(readxl::read_excel(path = path, skip = cells_parvo[1] + 4, col_names = FALSE)) %>%
    dplyr::rename_all(~ names_file) %>%
    ## this is a trick to drop NAs based on the last column
    ## the reason is that Parvo sometimes export additional info after the exported data at the end of the spreadsheet
    tidyr::drop_na(ncol(.)) %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::mutate_at(1, function(x) x * 60)

  ## the following will try to coerce the work rate column to numeric
  ## and it renames the given work rate column into 'work_rate'
  if(!is.null(work_rate_column)) {
    if(!work_rate_column %in% colnames(out)) {
      stop("It looks like the work rate column you chose does not exist.", call. = FALSE)
    } else {
      out <- dplyr::mutate(out, !!work_rate_column := as.numeric(!!rlang::sym(work_rate_column)))
      out <- dplyr::rename(out, work_rate = !!rlang::sym(work_rate_column))
    }
  }

  metadata <- NULL
  metadata$read_data <- TRUE
  metadata$metabolic_cart <- "Parvo Medics"
  metadata$data_status <- "raw data"
  metadata$time_column <- time_column

  out <- new_whippr_tibble(out, metadata)

  out
}

#' @export
read_data.geratherm <- function(
  path,
  metabolic_cart = c("cosmed", "cortex", "nspire", "parvo", "geratherm", "cardiocoach", "custom"),
  time_column = "t",
  work_rate_column = NULL
) {

  # if(purrr::is_empty(cells_parvo))
  #   stop("It looks like the name of the time column you chose does not exist.", call. = FALSE)

  ## retrieve column names
  names_file <- suppressMessages(readxl::read_excel(path = path, n_max = 1, col_names = TRUE)) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    dplyr::mutate(
      name = stringr::str_remove(string = name, pattern = "\\..*"),
      name = ifelse(is.na(value), name, paste(name, value))
    ) %>%
    dplyr::pull(name)

  ## it looks like this system starts with a column indicating additional info like warmup, etc.
  ## I will call it 'info' for now
  names_file <- c("info", names_file)

  ## read data again and adjust time column
  out <- suppressMessages(readxl::read_excel(path = path, skip = 4, col_names = FALSE)) %>%
    dplyr::rename_with(~ names_file) %>%
    dplyr::mutate(info = zoo::na.locf(info)) %>%
    dplyr::mutate(!!time_column := lubridate::hour(!!rlang::sym(time_column)) * 60 + lubridate::minute(!!rlang::sym(time_column)))

  ## it looks like this system restarts the time column depending on the 'info' column
  ## in this case we create an extra time column to try to fix that for the user
  out <- out %>%
    dplyr::group_by(info) %>%
    dplyr::mutate(time = !!rlang::sym(time_column) - dplyr::lag(!!rlang::sym(time_column), default = 0)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(time = cumsum(time)) %>%
    dplyr::select(!!rlang::sym(time_column), time, dplyr::everything())

  ## the following will try to coerce the work rate column to numeric
  ## and it renames the given work rate column into 'work_rate'
  if(!is.null(work_rate_column)) {
    if(!work_rate_column %in% colnames(out)) {
      stop("It looks like the work rate column you chose does not exist.", call. = FALSE)
    } else {
      out <- dplyr::mutate(out, !!work_rate_column := as.numeric(!!rlang::sym(work_rate_column)))
      out <- dplyr::rename(out, work_rate = !!rlang::sym(work_rate_column))
    }
  }

  metadata <- NULL
  metadata$read_data <- TRUE
  metadata$metabolic_cart <- "Geratherm"
  metadata$data_status <- "raw data"
  metadata$time_column <- time_column

  out <- new_whippr_tibble(out, metadata)

  out
}

#' @export
read_data.cardiocoach <- function(
  path,
  metabolic_cart = c("cosmed", "cortex", "nspire", "parvo", "geratherm", "cardiocoach", "custom"),
  time_column = "t",
  work_rate_column = NULL
) {
  ## check if readr is installed
  if(length(find.package(package = "readr", quiet = TRUE)) == 0) {
    stop("You need to install the readr package to use this function.", call. = FALSE)
  }
  ## retrieve column names
  names_file <- suppressMessages(readr::read_tsv(file = path, n_max = 2, col_names = FALSE))
  ## retrieve data
  readings <- suppressMessages(readr::read_tsv(file = path, col_names = FALSE, skip = 2, col_types = paste(c("c", replicate(ncol(names_file) - 1, "?")), collapse = "")))

  column_names <- dplyr::tibble(
    X1 = unlist(c(names_file[1,])),
    X2 = unlist(c(names_file[2,]))
  ) %>%
    dplyr::mutate(
      names = ifelse(is.na(X2), X1, glue::glue("{X1} ({X2})"))
    ) %>%
    dplyr::pull(names)

  ## define time column as 't' for simplicity
  column_names[1] <- "t"

  colnames(readings) <- column_names

  out <- readings %>%
    dplyr::mutate(
      t = dplyr::case_when(
        nchar(t) == 4 ~ paste0("00:0", t),
        nchar(t) == 5 ~ paste0("00:", t),
        TRUE ~ t
      ),
      t = lubridate::hms(t),
      t = lubridate::period_to_seconds(t)
    )

  metadata <- NULL
  metadata$read_data <- TRUE
  metadata$metabolic_cart <- "CardioCoach"
  metadata$data_status <- "raw data"
  metadata$time_column <- "t"

  out <- new_whippr_tibble(out, metadata)

  out
}

#' @export
read_data.custom <- function(
    path,
    metabolic_cart = c("cosmed", "cortex", "nspire", "parvo", "geratherm", "cardiocoach", "custom"),
    time_column = "t",
    work_rate_column = NULL
) {
  ## retrieve data
  readings <- suppressMessages(readxl::read_excel(path = path))

  ## define time column as 't' for simplicity
  column_names <- colnames(readings)
  column_names[1] <- "t"

  colnames(readings) <- column_names

  ## is the time column in seconds or in hms format?
  if(stringr::str_detect(string = readings[1,]$t, pattern = ":")) {
    readings <- readings %>%
      dplyr::mutate(
        t = as.character(t),
        t = dplyr::case_when(
          nchar(t) == 4 ~ paste0("00:0", t),
          nchar(t) == 5 ~ paste0("00:", t),
          TRUE ~ t
        ),
        t = lubridate::hms(t),
        t = lubridate::period_to_seconds(t)
      )
  }

  metadata <- NULL
  metadata$read_data <- TRUE
  metadata$metabolic_cart <- "Custom"
  metadata$data_status <- "raw data"
  metadata$time_column <- "t"

  out <- new_whippr_tibble(readings, metadata)

  out
}
