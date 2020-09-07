test_that("bin-averaging works", {
  path_example <- system.file("example_cosmed.xlsx", package = "whippr")

  df <- read_data(path = path_example, metabolic_cart = "cosmed")

  df_averaged <- df %>%
    interpolate() %>%
    perform_average(type = "bin", bins = 30)

  expect_s3_class(
    object = df_averaged,
    class = "whippr"
  )
})

test_that("rolling-averaging works", {
  path_example <- system.file("example_cosmed.xlsx", package = "whippr")

  df <- read_data(path = path_example, metabolic_cart = "cosmed")

  df_averaged <- df %>%
    interpolate() %>%
    perform_average(type = "rolling", rolling_window = 30)

  expect_s3_class(
    object = df_averaged,
    class = "whippr"
  )
})
