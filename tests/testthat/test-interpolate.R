test_that("interpolation works", {
  path_example <- system.file("example_cosmed.xlsx", package = "whippr")

  df <- read_data(path = path_example, metabolic_cart = "cosmed")

  df_interpolated <- df %>%
    interpolate()

  expect_s3_class(
    object = df_interpolated,
    class = "whippr"
  )
})
