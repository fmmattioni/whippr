test_that("core vo2 kinetics function works", {
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

  ## process data
  data_processed <- process_data(
    .data_outliers = data_outliers,
    protocol_baseline_length = 360,
    fit_bin_average = 5
  )

  data_kinetics <- perform_kinetics(
    .data_processed = data_processed,
    intensity_domain = "moderate",
    fit_level = 0.95,
    fit_phase_1_length = 20,
    fit_baseline_length = 120,
    fit_transition_length = 240,
    verbose =  FALSE
  )

  expect_s3_class(
    object = data_kinetics,
    class = "tbl"
  )
})
