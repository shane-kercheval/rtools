% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explore_basics.R
\name{rt_funnel_plot}
\alias{rt_funnel_plot}
\title{returns a funnel plot}
\usage{
rt_funnel_plot(
  step_names,
  step_values,
  title = "",
  subtitle = "",
  caption = "",
  proportionate = FALSE
)
}
\arguments{
\item{step_names}{the names of the steps, top down}

\item{step_values}{the values of the steps, top down; the first/top value is assumed to be 100 percent}

\item{title}{the title}

\item{subtitle}{the subtitle}

\item{caption}{the caption}

\item{proportionate}{controls the shape of the funnel
when FALSE, the width consistently decreases with each step
when TRUE, the width decreases proportionate to the value of each step}
}
\description{
returns a funnel plot
}
