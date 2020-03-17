# Get target data
target_cortex <- function(dat){
  target_cell <- which(dat == "t", arr.ind = TRUE)

  target_cell
}

target_cosmed <- function(dat){
  target_cell <- which(colnames(dat) == "t", arr.ind = TRUE)

  target_cell
}
