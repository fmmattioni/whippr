# Get target data
## these functions could be a single function actually,
## but perhaps it is better to keep it that way in case we add a system in the future that requires additional steps
target_cortex <- function(.data, time_column){
  target_cell <- which(.data == time_column, arr.ind = TRUE)

  ## usually cortex will not export the data at the top of the spreadsheet
  ## meaning that the data we are looking for won't have column names recognized here
  ## the following is just to make sure that, in case the user's export settings does export the data at the top of the spreadsheet, it will get recognized
  if(purrr::is_empty(target_cell))
    target_cell <- which(colnames(.data) == time_column, arr.ind = TRUE)

  target_cell
}

target_cosmed <- function(.data, time_column){
  target_cell <- which(colnames(.data) == time_column, arr.ind = TRUE)

  target_cell
}

target_nspire <- function(.data, time_column){
  target_cell <- which(colnames(.data) == time_column, arr.ind = TRUE)

  target_cell
}

target_parvo <- function(.data, time_column) {
  target_cell <- which(.data == time_column, arr.ind = TRUE)

  ## usually cortex will not export the data at the top of the spreadsheet
  ## meaning that the data we are looking for won't have column names recognized here
  ## the following is just to make sure that, in case the user's export settings does export the data at the top of the spreadsheet, it will get recognized
  if(purrr::is_empty(target_cell))
    target_cell <- which(colnames(.data) == time_column, arr.ind = TRUE)

  target_cell
}
