\name{TwopFeedbackModel14}
\alias{TwopFeedbackModel14}
\usage{TwopFeedbackModel14(t,
ks,
C0,
F0_Delta14C,
In,
a21,
a12,
xi=1,
inputFc,
lambda=-0.0001209681,
lag=0,
solver=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought. It must be specified within the same period for which the Delta 14 C of the atmosphere is provided. The default period in the provided dataset \code{\link{C14Atm_NH}} is 1900-2010.}
\item{ks}{A vector of length 2 containing the decomposition rates for the 2 pools.}
\item{C0}{A vector of length 2 containing the initial amount of carbon for the 2 pools.}
\item{F0_Delta14C}{A vector of length 2 containing the initial amount of the radiocarbon fraction for the 2 pools as Delta14C values in per mil.}
\item{In}{A scalar or a data.frame object specifying the amount of litter inputs by time.}
\item{a21}{A scalar with the value of the transfer rate from pool 1 to pool 2.}
\item{a12}{A scalar with the value of the transfer rate from pool 2 to pool 1.}
\item{xi}{A scalar or a data.frame specifying the external (environmental and/or edaphic) effects on decomposition rates.}
\item{inputFc}{A Data Frame object containing values of atmospheric Delta14C per time. First column must be time values, second column must be Delta14C values in per mil.}
\item{lambda}{Radioactive decay constant. By default lambda=-0.0001209681 y^-1 . This has the side effect that all your time related data are treated as if the time unit was year.}
\item{lag}{A positive integer representing a time lag for radiocarbon to enter the system.}
\item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.}
\item{pass}{Forces the constructor to create the model even if it is invalid}
}
\title{Implementation of a two-pool C14 model with feedback structure}
\description{This function creates a model for two pools connected with feedback. 
It is a wrapper for the more general function \code{\link{GeneralModel_14}} that can handle an arbitrary number of pools.}
\value{A Model Object that can be further queried }
