\name{Model_14}
\alias{Model_14}
\usage{Model_14(t,
A,
ivList,
initialValF,
inputFluxes,
inputFc,
c14DecayRate,
solverfunc=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought.}
\item{A}{something that can be converted by \link{GeneralDecompOp} to any of the available subclasses of \code{\linkS4class{DecompOp}}.}
\item{ivList}{A vector containing the initial amount of carbon for the n pools. The length of this vector is equal to the number of pools and thus equal to the length of k. This is checked by an internal  function.}
\item{initialValF}{An object equal or equivalent to class ConstFc containing a vector with the initial values of the radiocarbon fraction for each pool and a format string describing in which format the values are given.}
\item{inputFluxes}{something that can be converted by \link{GeneralInFlux} to any of the available subclasses of \linkS4class{InFlux}.}
\item{inputFc}{An object describing the fraction of C_14 in per mille (different formats are possible)}
\item{c14DecayRate}{the rate at which C_14 decays radioactivly. If you don't provide a value here we assume the following value: k=-0.0001209681 y^-1 . This has the side effect that all your time related data are treated as if the time unit was year. Thus beside time itself it also  affects decay rates the inputrates and the output}
\item{solverfunc}{The function used by to actually solve the ODE system. This can be \code{\link{deSolve.lsoda.wrapper}} or any other user provided function with the same interface.}
\item{pass}{Forces the constructor to create the model even if it is invalid}
}
\description{This method tries to create an object from any combination of arguments 
that can be converted into  the required set of building blocks for the Model_14
for n arbitrarily connected pools.}
\seealso{\code{\link{TwopParallelModel}}, \code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}} }
\title{general  constructor for class Model_14}
\value{A model object that can be further queried. }
