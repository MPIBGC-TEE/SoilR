\name{OnepModel}
\alias{OnepModel}
\usage{OnepModel(t,
k,
C0,
In,
xi=1,
solver=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought.}
\item{k}{A scalar with the decomposition rate of the pool.}
\item{C0}{A scalar containing the initial amount of carbon in the pool.}
\item{In}{A scalar or a data.frame object specifying the amount of litter inputs by time.}
\item{xi}{A scalar or a data.frame specifying the external (environmental and/or edaphic) effects on decomposition rates.}
\item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.}
\item{pass}{if TRUE forces the constructor to create the model even if it is invalid}
}
\description{This function creates a model for one pool. It is a wrapper for the more general function \code{\link{GeneralModel}}.}
\references{Sierra, C.A., M. Mueller, S.E. Trumbore. 2012. Models of soil organic matter decomposition: the SoilR package version 1.0. Geoscientific Model Development 5, 1045-1060.}
\seealso{\code{\link{TwopParallelModel}},\code{\link{TwopFeedbackModel}} }
\title{Implementation of a one pool model }
\value{A Model Object that can be further queried }
