% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explore_basics.R
\name{rt_value_counts}
\alias{rt_value_counts}
\title{returns a count of the values in a vector; the object returned is a dataframe}
\usage{
rt_value_counts(values)
}
\arguments{
\item{values}{a string vector}
}
\description{
returns a count of the values in a vector; the object returned is a dataframe
}
\examples{

library(ggplot2)
rt_value_counts(iris$Species)

}
