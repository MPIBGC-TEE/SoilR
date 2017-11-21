\name{plotCPool}
\alias{plotCPool}
\usage{plotCPool(t= ,
mat= ,
col= ,
...= )}
\arguments{
\item{t}{A vector containing the time points for plotting.}
\item{mat}{A matrix object obtained after a call to \code{\link{getC}} or \code{\link{getReleaseFlux}}}
\item{col}{A color palette specifying color lines for each pool (columns of \code{mat}).}
\item{...}{Other arguments passed to \code{link{plot}}.}
}
\description{This function produces a plot with the C content or released C for each pool over time. Needs as input a matrix obtained after a call to \code{\link{getC}} or \code{\link{getReleaseFlux}}.}
\title{Plots the output of \code{\link{getC}} or \code{\link{getReleaseFlux}} for each pool over time}
