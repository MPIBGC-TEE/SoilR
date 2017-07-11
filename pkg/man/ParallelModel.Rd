\name{ParallelModel}
\alias{ParallelModel}
\usage{ParallelModel(times,
coeffs_tm,
startvalues,
inputrates,
solverfunc=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{times}{A vector containing the points in time where the solution is sought.}
\item{coeffs_tm}{A TimeMap object consisting of a vector valued function containing the decay rates for the n pools as function of time and the time range where this function is valid. The length of the vector is equal to the number of pools.}
\item{startvalues}{A vector containing the initial amount of carbon for the n pools.
<<The length of this vector is equal to the number of pools and thus equal to the length of k. This is checked by the function.}
\item{inputrates}{An  object consisting of a vector valued function describing the inputs to the pools as funtions of time \code{\link{TimeMap.new}}}
\item{solverfunc}{The function used to actually solve the ODE system. This can be \code{\link{deSolve.lsoda.wrapper}} or any other user provided function with the same interface.}
\item{pass}{if TRUE forces the constructor to create the model even if it is invalid}
}
\description{This function creates a (linear) numerical model for n independent (parallel) pools that can be queried afterwards. 
It is used by the convinient wrapper functions \code{\link{TwopParallelModel}} and \code{\link{ThreepParallelModel}}
but can also be used independently.}
\title{models for unconnected pools}
\value{a model object}
