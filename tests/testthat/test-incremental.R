test_that("incremental_normalize ramp works", {
  ## get file path from example data
  path_example <- system.file("ramp_cosmed.xlsx", package = "whippr")
  ## read data from ramp test
  df <- read_data(path = path_example, metabolic_cart = "cosmed")
  ## normalize incremental test data
  ramp_normalized <- df %>%
    incremental_normalize(
      .data = .,
      incremental_type = "ramp",
      has_baseline = TRUE,
      baseline_length = 240,
      work_rate_magic = TRUE,
      baseline_intensity = 20,
      ramp_increase = 25
    )

  expect_s3_class(
    object = ramp_normalized,
    class = "tbl"
  )
})

test_that("incremental_normalize step works", {
  ## get file path from example data
  path_example <- system.file("step_cortex.xlsx", package = "whippr")
  ## read data from ramp test
  df <- read_data(path = path_example, metabolic_cart = "cortex")
  ## normalize incremental test data
  ramp_normalized <- df %>%
    incremental_normalize(
      .data = .,
      incremental_type = "step",
      has_baseline = TRUE,
      baseline_length = 120,
      work_rate_magic = TRUE,
      baseline_intensity = 0,
      step_start = 50,
      step_increase = 25,
      step_length = 180
    )

  expect_s3_class(
    object = ramp_normalized,
    class = "tbl"
  )
})

test_that("plot_incremental ramp works", {
  ## get file path from example data
  path_example <- system.file("ramp_cosmed.xlsx", package = "whippr")
  ## read data from ramp test
  df <- read_data(path = path_example, metabolic_cart = "cosmed")
  ## normalize incremental test data
  ramp_normalized <- df %>%
    incremental_normalize(
      .data = .,
      incremental_type = "ramp",
      has_baseline = TRUE,
      baseline_length = 240,
      work_rate_magic = TRUE,
      baseline_intensity = 20,
      ramp_increase = 25
    )

  ## plot
  expect_s3_class(
    plot_incremental(ramp_normalized),
    "ggplot"
  )
})

test_that("plot_incremental step works", {
  ## get file path from example data
  path_example <- system.file("step_cortex.xlsx", package = "whippr")
  ## read data from ramp test
  df <- read_data(path = path_example, metabolic_cart = "cortex")
  ## normalize incremental test data
  ramp_normalized <- df %>%
    incremental_normalize(
      .data = .,
      incremental_type = "step",
      has_baseline = TRUE,
      baseline_length = 120,
      work_rate_magic = TRUE,
      baseline_intensity = 0,
      step_start = 50,
      step_increase = 25,
      step_length = 180
    )

  ## plot
  expect_s3_class(
    plot_incremental(ramp_normalized),
    "ggplot"
  )
})
