\name{ThreepParallelModel}
\alias{ThreepParallelModel}
\usage{ThreepParallelModel(t,
ks,
C0,
In,
gam1,
gam2,
xi=1,
solver=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought.}
\item{ks}{A vector of length 3 containing the decomposition rates for the 3 pools.}
\item{C0}{A vector of length 3 containing the initial amount of carbon for the 3 pools.}
\item{In}{A scalar or a data.frame object specifying the amount of litter inputs by time.}
\item{gam1}{A scalar representing the partitioning coefficient, i.e. the proportion from the total amount of inputs that goes to pool 1.}
\item{gam2}{A scalar representing the partitioning coefficient, i.e. the proportion from the total amount of inputs that goes to pool 2.}
\item{xi}{A scalar or a data.frame specifying the external (environmental and/or edaphic) effects on decomposition rates.}
\item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.}
\item{pass}{see the method arguments for details}
}
\title{Implementation of a three pool model with parallel structure}
\description{The function creates a model for three independent (parallel) pools. It is a wrapper for the more general function
\code{\link{ParallelModel}} that can handle an arbitrary number of pools.}
\references{Sierra, C.A., M. Mueller, S.E. Trumbore. 2012. Models of soil organic matter decomposition: the SoilR package version 1.0. Geoscientific Model Development 5, 1045-1060.}
\value{A  Model Object that can be further queried}
