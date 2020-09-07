test_that("general vo2 kinetics function works", {
  path_example <- system.file("example_cosmed.xlsx", package = "whippr")

  ## read data
  df <- read_data(path = path_example, metabolic_cart = "cosmed", time_column = "t")

  ## VO2 kinetics analysis
  results_kinetics <- vo2_kinetics(
    .data = df,
    intensity_domain = "moderate",
    vo2_column = "VO2",
    protocol_n_transitions = 3,
    protocol_baseline_length = 360,
    protocol_transition_length = 360,
    cleaning_level = 0.95,
    cleaning_baseline_fit = c("linear", "exponential", "exponential"),
    fit_level = 0.95,
    fit_bin_average = 5,
    fit_phase_1_length = 20,
    fit_baseline_length = 120,
    fit_transition_length = 240,
    verbose = TRUE
  )

  expect_s3_class(
    object = results_kinetics,
    class = "tbl"
  )
})
