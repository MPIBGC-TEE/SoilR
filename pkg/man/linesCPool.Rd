\name{linesCPool}
\alias{linesCPool}
\usage{linesCPool(t,
mat,
col,
...)}
\arguments{
\item{t}{A vector containing the time points for plotting.}
\item{mat}{A matrix object obtained after a call to \code{\link{getC14}}, \code{\link{getC}}, or \code{\link{getReleaseFlux}}.}
\item{col}{A color palette specifying color lines for each pool (columns of \code{mat}).}
\item{...}{Other arguments passed to \code{plot}.}
}
\description{This function adds lines to a plot with the C content, the C release, or Delta 14C value of each pool over time. Needs as input a matrix obtained after a call to \code{\link{getC14}}, \code{\link{getC}}, or \code{\link{getReleaseFlux}}.}
\title{Add lines with the output of \code{\link{getC14}}, \code{\link{getC}}, or \code{\link{getReleaseFlux}} to an existing plot}
