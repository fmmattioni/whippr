% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers-incremental.R
\name{work_rate_step}
\alias{work_rate_step}
\title{Work rate for a step-incremental test}
\usage{
work_rate_step(
  .data,
  baseline_intensity,
  step_start,
  step_increase,
  step_length
)
}
\arguments{
\item{.data}{The data with recognized protocol phases}

\item{baseline_intensity}{The baseline intensity}

\item{step_start}{In case the step test started in a different work rate than baseline}

\item{step_increase}{The step in increase, in watts per step}

\item{step_length}{The length, in seconds, of each step}
}
\value{
a \link[tibble:tibble-package]{tibble}
}
\description{
This function produces the work rate throughout a step-incremental test given the protocol
This will retrieve both the 'original' work rates, and also will perform a 'linearization' of the steps.
}
\keyword{internal}
