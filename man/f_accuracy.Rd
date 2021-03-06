% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_accuracy.r
\name{f_accuracy}
\alias{f_accuracy}
\title{Calculate the accuracy of time series}
\usage{
f_accuracy(insample, outsample, forecasts, method = c("mase", "smape"))
}
\arguments{
\item{insample}{series that input the model.}

\item{outsample}{hope to predict the phase of the series.}

\item{forecasts}{the result of the model.}

\item{method}{which metric to calculate the accuracy.}
}
\value{
A list with the elements having the following structure
}
\description{
Calculate the accuracy of time series
}
