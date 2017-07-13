\name{OnepModel14}
\alias{OnepModel14}
\usage{OnepModel14(t,
k,
C0,
F0_Delta14C,
In,
xi=1,
inputFc,
lambda=-0.0001209681,
lag=0,
solver=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought. It must be specified within the same period for which the Delta 14 C of the atmosphere is provided. The default period in the provided dataset \code{\link{C14Atm_NH}} is 1900-2010.}
\item{k}{A scalar with the decomposition rate of the pool.}
\item{C0}{A scalar containing the initial amount of carbon in the pool.}
\item{F0_Delta14C}{A scalar containing the initial amount of the radiocarbon fraction in the pool in Delta_14C format.}
\item{In}{A scalar or a data.frame object specifying the amount of litter inputs by time.}
\item{xi}{A scalar or a data.frame specifying the external (environmental and/or edaphic) effects on decomposition rates.}
\item{inputFc}{A Data Frame object consisting of  a function describing the fraction of C_14 in per mille. The first column will be assumed to contain the times.}
\item{lambda}{Radioactive decay constant. By default lambda=-0.0001209681 y^-1 . This has the side effect that all your time related data are treated as if the time unit was year.}
\item{lag}{A (positive) scalar representing a time lag for radiocarbon to enter the system.}
\item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.}
\item{pass}{if TRUE Forces the constructor to create the model even if it is invalid}
}
\description{This function creates a model for one pool. It is a wrapper for the more general function \code{\link{GeneralModel_14}}. }
\seealso{\code{\link{OnepModel}}, \code{\link{TwopParallelModel14}}, 
\code{\link{TwopFeedbackModel14}} 
examples<<

years=seq(1901,2009,by=0.5)
LitterInput=700 

Ex=OnepModel14(t=years,k=1/10,C0=500, F0=0,In=LitterInput, inputFc=C14Atm_NH)
C14t=getF14(Ex)

plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years, C14t[,1], col=4)
legend(
"topright",
c("Delta 14C Atmosphere", "Delta 14C in SOM"),
lty=c(1,1),
col=c(1,4),
lwd=c(1,1),
bty="n"
)}
\title{Implementation of a one-pool C14 model}
