% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{normalize_time}
\alias{normalize_time}
\title{Normalize time column}
\usage{
normalize_time(.data, protocol_baseline_length)
}
\arguments{
\item{.data}{Breath-by-breath data.}

\item{protocol_baseline_length}{The length of the baseline (in seconds).}
}
\value{
a \link[tibble:tibble-package]{tibble}
}
\description{
Normalizes the the time column such that the baseline phase has negative time values. Point zero will then represent the start of the transition phase.
}
