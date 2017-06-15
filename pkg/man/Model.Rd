\name{Model}
\alias{Model}
\usage{Model(t,
A,
ivList,
inputFluxes,
solverfunc=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought.}
\item{A}{something that can be converted to any of the available subclasses of \code{\link{DecompOp-class}}. Take a look at}
\item{ivList}{A numeric vector containing the initial amount of carbon for the n pools. The length of this vector is equal to the number of pools .This is checked by an internal  function.}
\item{inputFluxes}{something that can be converted to any of the available subclasses of \code{\link{InFlux}}.}
\item{solverfunc}{The function used by to actually solve the ODE system. This can be \code{\link{deSolve.lsoda.wrapper}} or any other user provided function with the same interface.}
\item{pass}{Forces the constructor to create the model even if it is invalid}
}
\title{general  constructor for class \code{\link{Model}}}
\description{This method tries to create a Model object from any combination of arguments 
that can be converted into  the required set of building blocks for a model
for n arbitrarily connected pools.
It does so by converting }
\value{A model object that can be further queried. }
