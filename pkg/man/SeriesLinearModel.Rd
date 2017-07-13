\name{SeriesLinearModel}
\alias{SeriesLinearModel}
\usage{SeriesLinearModel(t,
m.pools,
ki,
Tij,
C0,
In,
xi=1,
solver=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought.}
\item{m.pools}{An integer with the total number of pools in the model.}
\item{ki}{A vector of lenght m containing the values of the decomposition rates for each pool i.}
\item{Tij}{A vector of length m-1 with the transfer coefficients from pool j to pool i. The value of these coefficients must be in the range [0, 1].}
\item{C0}{A vector of length m containing the initial amount of carbon for the m pools.}
\item{In}{A scalar or data.frame object specifying the amount of litter inputs by time.}
\item{xi}{A scalar or data.frame object specifying the external (environmental and/or edaphic) effects on decomposition rates.}
\item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.}
\item{pass}{if TRUE Forces the constructor to create the model even if it is invalid}
}
\description{This function creates a model for m number of pools connected in series. It is a wrapper for the more general function \code{\link{GeneralModel}}.}
\references{Sierra, C.A., M. Mueller, S.E. Trumbore. 2012. Models of soil organic matter decomposition: the SoilR package version 1.0. Geoscientific Model Development 5, 1045-1060.}
\seealso{\code{\link{GeneralModel}}, \code{\link{ThreepFeedbackModel}}, \code{\link{ParallelModel}} 
examples<<
#A five-pool model
t_start=0 
t_end=10 
tn=50
timestep=(t_end-t_start)/tn 
t=seq(t_start,t_end,timestep) 
ks=c(k1=0.8,k2=0.4,k3=0.2, k4=0.1,k5=0.05)
Ts=c(0.5,0.2,0.2,0.1)
C0=c(C10=100,C20=150, C30=50, C40=50, C50=10)
In = 50

Ex1=SeriesLinearModel(t=t,m.pools=5,ki=ks,Tij=Ts,C0=C0,In=In,xi=fT.Q10(15))
Ct=getC(Ex1)

matplot(t,Ct,type="l",col=2:6,lty=1,ylim=c(0,sum(C0)))
lines(t,rowSums(Ct),lwd=2)
legend("topright",c("Total C","C in pool 1", "C in pool 2","C in pool 3",
"C in pool 4","C in pool 5"),
lty=1,col=1:6,lwd=c(2,rep(1,5)),bty="n")}
\title{General m-pool linear model with series structure}
