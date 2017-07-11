\name{TwopSeriesModel}
\alias{TwopSeriesModel}
\usage{TwopSeriesModel(t,
ks,
a21,
C0,
In,
xi=1,
solver=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought.}
\item{ks}{A vector of length 2 with the values of the decomposition rate for pools 1 and 2.}
\item{a21}{A scalar with the value of the transfer rate from pool 1 to pool 2.}
\item{C0}{A vector of length 2 containing the initial amount of carbon for the 2 pools.}
\item{In}{A scalar or a data.frame object specifying the amount of litter inputs by time.}
\item{xi}{A scalar or a data.frame specifying the external (environmental and/or edaphic) effects on decomposition rates.}
\item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.}
\item{pass}{if TRUE Forces the constructor to create the model even if it is invalid}
}
\description{This function creates a model for two pools connected in series. It is a wrapper for the more general function \code{\link{GeneralModel}}.}
\references{Sierra, C.A., M. Mueller, S.E. Trumbore. 2012. Models of soil organic matter decomposition: the SoilR package version 1.0. Geoscientific Model Development 5, 1045-1060.}
\seealso{\code{\link{TwopParallelModel}},\code{\link{TwopFeedbackModel}} }
\title{Implementation of a two pool model with series structure}
\value{A Model Object that can be further queried }
