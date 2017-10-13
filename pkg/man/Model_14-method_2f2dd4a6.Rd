\docType{methods}
\name{Model_14,numeric,ANY,numeric,ConstFc,ANY,ANY,numeric,function,logical-method}
\title{Model_14,numeric,ANY,numeric,ConstFc,ANY,ANY,numeric,function,logical-method}
\alias{Model_14,numeric,ANY,numeric,ConstFc,ANY,ANY,numeric,function,logical-method}
\usage{\S4method{Model_14}{numeric,ANY,numeric,ConstFc,ANY,ANY,numeric,function,logical}(t,
A,
ivList,
initialValF,
inputFluxes,
inputFc,
c14DecayRate,
solverfunc,
pass=FALSE)}
\arguments{
\item{t}{: of class numeric, A vector containing the points in time where the solution is sought.}
\item{A}{: of class ANY, something that can be converted by \link{GeneralDecompOp} to any of the available subclasses of \code{\link{DecompOp-class}}.}
\item{ivList}{: of class numeric, A vector containing the initial amount of carbon for the n pools. The length of this vector is equal to the number of pools and thus equal to the length of k. This is checked by an internal  function.}
\item{initialValF}{: of class ConstFc, An object equal or equivalent to class ConstFc containing a vector with the initial values of the radiocarbon fraction for each pool and a format string describing in which format the values are given.}
\item{inputFluxes}{: of class ANY, something that can be converted by \link{GeneralInFlux} to any of the available subclasses of \link{InFlux-class}.}
\item{inputFc}{: of class ANY, An object describing the fraction of C_14 in per mille (different formats are possible)}
\item{c14DecayRate}{: of class numeric}
\item{solverfunc}{: of class function, The function used by to actually solve the ODE system. This can be \code{\link{deSolve.lsoda.wrapper}} or any other user provided function with the same interface.}
\item{pass}{: of class logical, Forces the constructor to create the model even if it is invalid}
}
\description{This method tries to create an object from any combination of arguments 
that can be converted into  the required set of building blocks for the Model_14
for n arbitrarily connected pools.}
