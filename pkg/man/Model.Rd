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
\item{solverfunc}{The function used to actually solve the ODE system. This can be \code{\link{deSolve.lsoda.wrapper}} or any other user provided function with the same interface.}
\item{pass}{Forces the constructor to create the model even if it does not pass internal sanity checks}
}
\description{This function creates an object of class \link{Model-class}, 
The arguments can be given in different form as long as they can 
be converted to the necessary internal building blocks. 
(See the links)}
\details{The internal constructor of class \link{Model-class} requires the argument \code{A} to be of class \link{DecompOp-class} and argument \code{inputFluxes} to be of  class \link{InFlux-class}.
Before calling the internal constructor this function calls \link{DecompOpSubClassInstance} on its argument \code{A} and  \link{InFluxSubClassInstance} on its argument \code{inputFluxes} to convert them into
the required classes.
Both are generic functions. Follow the links to see for which kind of inputs conversion methods are available.
The attempted conversion allows great flexibility with respect to arguments and independence from the actual implementation.
However if your code uses the wrong argument the error will most
likely occur in the delegate functions. 
If this happens use \code{traceback()} to see which function was called
and try to call the constructor of the desired subclass 
explicitly with your arguments. 
The subclasses are linked in the class documentation \link{DecompOp-class} or \link{InFlux-class} respectively.}
\seealso{This function is called by many of the \link{predefinedModels} }
\title{Constructor for class \link{Model-class} }
\value{An object of class \link{Model-class} that can be queried by many methods 
to be found there.}
