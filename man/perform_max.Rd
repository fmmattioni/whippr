% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/max.R
\name{perform_max}
\alias{perform_max}
\title{Perform VO2max calculation}
\usage{
perform_max(
  .data,
  vo2_column = "VO2",
  vo2_relative_column = NULL,
  heart_rate_column = NULL,
  rer_column = NULL,
  average_method = c("bin", "rolling"),
  average_length = 30,
  plot = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{.data}{The data retrieved either from \code{incremental_normalize()} or \code{detect_outliers()}.}

\item{vo2_column}{The name (quoted) of the column containing the absolute oxygen uptake (VO2) data. Default to \code{"VO2"}.}

\item{vo2_relative_column}{The name (quoted) of the column containing the relative to body weight oxygen uptake (VO2) data. Default to \code{NULL}.}

\item{heart_rate_column}{The name (quoted) of the column containing the heart rate (HR) data. Default to \code{NULL}. If \code{NULL}, this parameter will not be calculated.}

\item{rer_column}{The name (quoted) of the column containing the respiratory exchange ratio (RER) data. Default to \code{NULL}. If \code{NULL}, this parameter will not be calculated.}

\item{average_method}{The average method to be used for VO2max calculation. One of \code{bin} or \code{rolling}.}

\item{average_length}{The length, in seconds, of the average to be used. For example, if \code{average_method = bin}, and \code{average_length = 30}, it will perform a 30-s bin-average.}

\item{plot}{A boolean indicating whether to produce a plot with the summary results. Default to \code{TRUE}.}

\item{verbose}{A boolean indicating whether messages should be printed in the console. Default to \code{TRUE}.}
}
\value{
a tibble
}
\description{
It performs the calculation of VO2max, HRmax, and maximal RER. Additionally, it detects whether a plateau can be identified from your data.
}
\examples{
\dontrun{
## get file path from example data
path_example <- system.file("ramp_cosmed.xlsx", package = "whippr")

## read data from ramp test
df <- read_data(path = path_example, metabolic_cart = "cosmed")

## normalize incremental test data
ramp_normalized <- df \%>\%
 incremental_normalize(
   .data = .,
   incremental_type = "ramp",
   has_baseline = TRUE,
   baseline_length = 240,
   work_rate_magic = TRUE,
   baseline_intensity = 20,
   ramp_increase = 25
 )

## detect outliers
data_ramp_outliers <- detect_outliers(
 .data = ramp_normalized,
 test_type = "incremental",
 vo2_column = "VO2",
 cleaning_level = 0.95,
 method_incremental = "linear",
 verbose = TRUE
)

## analyze VO2max
perform_max(
 .data = data_ramp_outliers,
 vo2_column = "VO2",
 vo2_relative_column = "VO2/Kg",
 heart_rate_column = "HR",
 rer_column = "R",
 average_method = "bin",
 average_length = 30,
 plot = TRUE,
 verbose = FALSE
)
}
}
