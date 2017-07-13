\name{SeriesLinearModel14}
\alias{SeriesLinearModel14}
\usage{SeriesLinearModel14(t,
m.pools,
ki,
Tij,
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
\item{t}{A vector containing the points in time where the solution is sought.}
\item{m.pools}{An integer with the total number of pools in the model.}
\item{ki}{A vector of lenght m containing the values of the decomposition rates for each pool i.}
\item{Tij}{A vector of length m-1 with the transfer coefficients from pool j to pool i. The value of these coefficients must be in the range [0, 1].}
\item{C0}{A vector of length m containing the initial amount of carbon for the m pools.}
\item{F0_Delta14C}{A vector of length m containig the initial amount of the radiocarbon fraction for the m pools.}
\item{In}{A scalar or data.frame object specifying the amount of litter inputs by time.}
\item{xi}{A scalar or data.frame object specifying the external (environmental and/or edaphic) effects on decomposition rates.}
\item{inputFc}{A Data Frame object containing values of atmospheric Delta14C per time. First column must be time values, second column must be Delta14C values in per mil.}
\item{lambda}{Radioactive decay constant. By default lambda=-0.0001209681 y^-1 . This has the side effect that all your time related data are treated as if the time unit was year.}
\item{lag}{A positive scalar representing a time lag for radiocarbon to enter the system.}
\item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.}
\item{pass}{if TRUE Forces the constructor to create the model even if it is invalid}
}
\description{This function creates a radiocarbon model for m number of pools connected in series. It is a wrapper for the more general function \code{\link{GeneralModel_14}}.}
\examples{years=seq(1901,2009,by=0.5)
LitterInput=700 

Ex=SeriesLinearModel14(
t=years,ki=c(k1=1/2.8, k2=1/35, k3=1/100), m.pools=3,
C0=c(200,5000,500), F0_Delta14C=c(0,0,0),
In=LitterInput, Tij=c(0.5, 0.1),inputFc=C14Atm_NH
)
R14m=getF14R(Ex)
C14m=getF14C(Ex)
C14t=getF14(Ex)

par(mfrow=c(2,1))
plot(C14Atm_NH,type="l",xlab="Year",
ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years, C14t[,1], col=4)
lines(years, C14t[,2],col=4,lwd=2)
lines(years, C14t[,3],col=4,lwd=3)
legend(
"topright",
c("Delta 14C Atmosphere", "Delta 14C pool 1", "Delta 14C pool 2", "Delta 14C pool 3"),
lty=rep(1,4),col=c(1,4,4,4),lwd=c(1,1,2,3),bty="n")

plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years,C14m,col=4)
lines(years,R14m,col=2)
legend("topright",c("Delta 14C Atmosphere","Delta 14C SOM", "Delta 14C Respired"),
lty=c(1,1,1), col=c(1,4,2),bty="n")
par(mfrow=c(1,1))}
\references{Sierra, C.A., M. Mueller, S.E. Trumbore. 2014. Modeling radiocarbon dynamics in soils: SoilR version 1.1. Geoscientific Model Development 7, 1919-1931.}
\seealso{\code{\link{GeneralModel_14}}, \code{\link{SeriesLinearModel}} }
\title{General m-pool linear C14 model with series structure}
