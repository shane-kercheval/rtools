% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explore_basics.R
\name{rt_explore_correlations}
\alias{rt_explore_correlations}
\title{returns a matrix containing of correlations}
\usage{
rt_explore_correlations(
  dataset,
  corr_threshold = 0,
  p_value_threshold = 1,
  max_missing_column_perc = 0.25,
  type = "pearson"
)
}
\arguments{
\item{dataset}{dataframe containing numberic columns}

\item{corr_threshold}{any correlations that are <= `corr_threshold` will be set to `NA`. (Default is `0`, so all correlations are shown). Helps to reduce noise.}

\item{p_value_threshold}{any correlations that have a p-value greater than `p_value_threshold` will be set to `NA` (Default is `1`, so all correlations are shown)}

\item{max_missing_column_perc}{the max percent of missing values for a column for it to be included.
The default is `0.25` (i.e. `25%`) meaning any column with more than 25% of it's data missing will be
removed.
For example, if you set the value to `0.05` (i.e. `5%` then the any column that had more than 5% of
values missing would be removed.}

\item{type}{type of correlation to perform (Default is `pearson`)}
}
\description{
returns a matrix containing of correlations
}
\examples{

library(ggplot2)
rt_explore_correlations(iris)

}
