\name{plotC14Pool}
\alias{plotC14Pool}
\usage{plotC14Pool(t,
mat,
inputFc,
col,
...)}
\arguments{
\item{t}{A vector containing the time points for plotting.}
\item{mat}{A matrix object obtained after a call to \code{\link{getF14}}}
\item{inputFc}{A Data Frame object containing values of atmospheric Delta14C per time. First column must be time values, second column must be Delta14C values in per mil.}
\item{col}{A color palette specifying color lines for each pool (columns of \code{mat}).}
\item{...}{Other arguments passed to \code{plot}.}
}
\description{This function produces a plot with the Delta14C in the atmosphere and the Delta14C of each pool obtained after a call to \code{\link{getF14}}.}
\title{Plots the output of \code{\link{getF14}} for each pool over time}
