% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Qfit.R
\name{outlier_filter}
\alias{outlier_filter}
\title{Helper function to filter outliers from regression parameters using interquartile range}
\usage{
outlier_filter(params, outlier_quantile, wopt = list())
}
\arguments{
\item{params}{regression parameters for fitted surface}

\item{outlier_quantile}{A numeric vector of length two or three. If two numbers are used it specifies the lower (Q1) and upper (Q2) quantiles used for determining the interquantile range (IQR). These values should be between 0 and 1 with Q2 > Q1. An optional third number can be used to specify a the size of a regular sample to be taken which can be useful if the full dataset is too large to fit in memory. Values are considered outliers if they are less than Q1-(100\emph{IQR) or greater than Q2+(100}IQR), where IQR=Q2-Q1.}

\item{wopt}{list with named options for writing files as in writeRaster}
}
\description{
Helper function to filter outliers from regression parameters using interquartile range
}
