% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{rt_regression}
\alias{rt_regression}
\title{Builds a (string) formula to pass to lm}
\usage{
rt_regression(
  dataset,
  dependent_variable,
  independent_variables,
  interaction_variables = NULL
)
}
\arguments{
\item{dataset}{dataset}

\item{dependent_variable}{dependent_variable}

\item{independent_variables}{independent_variables}

\item{interaction_variables}{list with elements as character vectors. each element is an interaction}
}
\description{
Builds a (string) formula to pass to lm
}
