#' Read data from metabolic cart
#'
#' It reads the raw data exported from the metabolic cart.
#'
#' @param path Path to read the file from.
#' @param metabolic_cart Metabolic cart that was used for data collection. Currently, only 'cosmed' and 'cortex' are supported.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
read_data <- function(path, metabolic_cart = c("cosmed", "cortex")) {
  if(missing(metabolic_cart))
    stop("You must specify the metabolic cart.", call. = FALSE)

  metabolic_cart <- match.arg(metabolic_cart)

  class(path) <- metabolic_cart

  UseMethod("read_data", path)
}

#' @export
read_data.cosmed <- function(path, metabolic_cart = c("cosmed", "cortex")) {
  data_raw <- suppressMessages(readxl::read_excel(path = path))

  ## find column that starts the data (time column will always be the first one)
  start_col <- target_cosmed(data_raw)

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
    col_types = c(rep("guess", num_cols_pre), "date", rep("numeric", num_cols_post))
  ))) %>%
    dplyr::select(start_col:ncol(.)) %>%
    dplyr::rename_all(~ names_file) %>%
    janitor::remove_empty(which = "rows")

  ## if date parsing didnt't work, then let the read_excel function guess the type
  if(all(is.na(data_raw2$t))) {
    ## read data again, select only columns with data and apply column names
    data_raw2 <- suppressMessages(readxl::read_excel(path = path, skip = 1)) %>%
      dplyr::select(start_col:ncol(.)) %>%
      dplyr::rename_all(~ names_file) %>%
      janitor::remove_empty(which = "rows")

    ## this will make sure that different versions of the cosmed will work with this function
    ## newer versions will display differnt time formats (00:00 instead of 00:00:00)
    if(all(nchar(data_raw2$t) == 5)){
      data_raw2$t <- paste0("00:", data_raw2$t)
    }

    out <- data_raw2 %>%
      dplyr::mutate(t = stringr::str_replace_all(t, ",", "."),
                    t = lubridate::hms(t),
                    t = lubridate::period_to_seconds(t),
                    t = as.integer(t))
  } else {
    out <- data_raw2 %>%
      dplyr::mutate(t = (lubridate::hour(t) * 3600) + (lubridate::minute(t) * 60) + lubridate::second(t),
                    t = as.integer(t))
  }

  out
}

#' @export
read_data.cortex <- function(path, metabolic_cart = c("cosmed", "cortex")) {
  data_raw <- suppressMessages(readxl::read_excel(path = path))

  ## find column that starts the data (time column will always be the first one)
  cells_cortex <- target_cortex(data_raw)

  ## retrieve column names
  names_file <- data_raw[cells_cortex[1],] %>%
    unlist(use.names = FALSE)

  ## read data again specifying starting row
  data_raw2 <- suppressMessages(readxl::read_excel(path = path, skip = cells_cortex[1] + 3, col_names = FALSE)) %>%
    dplyr::rename_all(~ names_file) %>%
    dplyr::mutate_if(is.character, ~ stringr::str_replace_all(., ",", "."))

  ## if time column was parsed as a date
  if(all(class(data_raw2$t) %in% c("POSIXct", "POSIXt"))) {
    data_raw2 <- data_raw2 %>%
      dplyr::mutate(t = (lubridate::hour(t) * 3600) + (lubridate::minute(t) * 60) + lubridate::second(t))
  } else {
    ## if time column was parsed as numeric
    if(any(stringr::str_detect(data_raw2$t, ":"))){
      data_raw2 <- data_raw2 %>%
        dplyr::mutate(t = lubridate::hms(t),
                      t = lubridate::period_to_seconds(t))
    } else{
      data_raw2 <- data_raw2 %>%
        dplyr::mutate(t = as.numeric(t),
                      t = t * 60 * 60 * 24)
    }
  }

  out <- data_raw2 %>%
    janitor::remove_empty(which = "cols")

  out
}
