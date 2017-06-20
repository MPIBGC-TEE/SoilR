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
\item{A}{something that can be converted by \link{DecompOpSubClassInstance} to any of the available subclasses of \code{\link{DecompOp-class}}.}
\item{ivList}{A numeric vector containing the initial amount of carbon for the n pools. The length of this vector is equal to the number of pools .This is checked by an internal  function.}
\item{inputFluxes}{something that can be converted by \link{InFluxSubClassInstance} to any of the available subclasses of \link{InFlux-class}.}
\item{solverfunc}{The function used by to actually solve the ODE system. This can be \code{\link{deSolve.lsoda.wrapper}} or any other user provided function with the same interface.}
\item{pass}{Forces the constructor to create the model even if it is invalid}
}
\title{Constructor for class \code{\link{Model-class}}}
\description{This function creates a Model object from any combination of arguments 
that can be converted into  the required set of building blocks for a model
for n arbitrarily connected pools.}
\details{The function calls \link{DecompOpSubClassInstance} on its \code{A} argument and  \link{InFluxSubClassInstance} on its \code{inputFluxes} argument. Both are generic functions with several methods for different classes of objects. Any combination of those classes can be used here. Follow the links to see a list of the available method.}
\seealso{\code{\link{TwopParallelModel}}, \code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}} }
\value{A model object that can be further queried. }
