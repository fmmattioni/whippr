% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{normalize_transitions}
\alias{normalize_transitions}
\title{Normalize transitions}
\usage{
normalize_transitions(
  .data,
  protocol_n_transitions,
  protocol_baseline_length,
  protocol_transition_length
)
}
\arguments{
\item{.data}{Breath-by-breath data.}

\item{protocol_n_transitions}{Number of transitions performed.}

\item{protocol_baseline_length}{The length of the baseline (in seconds).}

\item{protocol_transition_length}{The length of the transition (in seconds).}
}
\value{
a \link[tibble:tibble-package]{tibble}
}
\description{
Recognizes and normalizes the time column of each transition. It will also label the transitions into: 'baseline' or 'transition'.
}
