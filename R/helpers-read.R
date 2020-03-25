# Get target data
target_cortex <- function(.data, time_column){
  target_cell <- which(.data == time_column, arr.ind = TRUE)

  target_cell
}

target_cosmed <- function(.data, time_column){
  target_cell <- which(colnames(.data) == time_column, arr.ind = TRUE)

  target_cell
}
