% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_histogram}
\alias{plot_histogram}
\title{Plot a Histogram}
\usage{
plot_histogram(x)
}
\arguments{
\item{x}{A numeric vector.}
}
\value{
A ggplot2 histogram object.
}
\description{
This function creates a histogram of a numeric vector using ggplot2.
}
\examples{
plot_histogram(mtcars$mpg)
}
