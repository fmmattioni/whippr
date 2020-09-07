test_that("detection of outliers works", {
  path_example <- system.file("example_cosmed.xlsx", package = "whippr")

  df <- read_data(path = path_example, metabolic_cart = "cosmed")

  ## detect outliers
  data_outliers <- detect_outliers(
    .data = df,
    test_type = "kinetics",
    vo2_column = "VO2",
    cleaning_level = 0.95,
    cleaning_baseline_fit = c("linear", "exponential", "exponential"),
    protocol_n_transitions = 3,
    protocol_baseline_length = 360,
    protocol_transition_length = 360,
    verbose = FALSE
  )

  expect_s3_class(
    object = data_outliers,
    class = "whippr"
  )
})
