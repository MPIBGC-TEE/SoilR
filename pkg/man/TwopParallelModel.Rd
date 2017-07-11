\name{TwopParallelModel}
\alias{TwopParallelModel}
\usage{TwopParallelModel(t,
ks,
C0,
In,
gam,
xi=1,
solver=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought.}
\item{ks}{A vector of length 2 containing the decomposition rates for the 2 pools.}
\item{C0}{A vector of length 2 containing the initial amount of carbon for the 2 pools.}
\item{In}{A scalar or a data.frame object specifying the amount of litter inputs by time.}
\item{gam}{A scalar representing the partitioning coefficient, i.e. the proportion from the total amount of inputs that goes to pool 1.}
\item{xi}{A scalar or a data.frame specifying the external (environmental and/or edaphic) effects on decomposition rates.}
\item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.}
\item{pass}{Forces the constructor to create the model even if it is invalid}
}
\description{This function creates a model for two independent (parallel) pools. 
It is a wrapper for the more general function \code{\link{ParallelModel}} that can handle an arbitrary number of pools.}
\references{Sierra, C.A., M. Mueller, S.E. Trumbore. 2012. Models of soil organic matter decomposition: the SoilR package version 1.0. Geoscientific Model Development 5, 1045-1060.}
\seealso{\code{\link{ThreepParallelModel}} }
\title{Implementation of a linear two pool model with parallel structure}
\value{A Model Object that can be further queried }
