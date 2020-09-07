test_that("read data works", {
  path_example <- system.file("example_cosmed.xlsx", package = "whippr")

  df <- read_data(path = path_example, metabolic_cart = "cosmed")

  expect_s3_class(
    object = df,
    class = "whippr"
  )
})
