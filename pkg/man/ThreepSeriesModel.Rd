\name{ThreepSeriesModel}
\alias{ThreepSeriesModel}
\usage{ThreepSeriesModel(t,
ks,
a21,
a32,
C0,
In,
xi=1,
solver=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought.}
\item{ks}{A vector of lenght 3 containing the values of the decomposition rates for pools 1, 2, and 3.}
\item{a21}{A scalar with the value of the transfer rate from pool 1 to pool 2.}
\item{a32}{A scalar with the value of the transfer rate from pool 2 to pool 3.}
\item{C0}{A vector of length 3 containing the initial amount of carbon for the 3 pools.}
\item{In}{A scalar or data.frame object specifying the amount of litter inputs by time.}
\item{xi}{A scalar or data.frame object specifying the external (environmental and/or edaphic) effects on decomposition rates.}
\item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.}
\item{pass}{if TRUE Forces the constructor to create the model even if it is invalid}
}
\title{Implementation of a three pool model with series structure}
\description{This function creates a model for three pools connected in series. It is a wrapper for the more general function \code{\link{GeneralModel}}.}
\references{Sierra, C.A., M. Mueller, S.E. Trumbore. 2012. Models of soil organic matter decomposition: the SoilR package version 1.0. Geoscientific Model Development 5, 1045-1060.}
\seealso{\code{\link{ThreepParallelModel}}, \code{\link{ThreepFeedbackModel}} }
\value{A Model Object that can be further queried }
