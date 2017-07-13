\name{RothCModel}
\alias{RothCModel}
\usage{RothCModel(t,
ks=c(k.DPM = 10, k.RPM = 0.3, k.BIO = 0.66, k.HUM = 0.02, k.IOM = 0),
C0=c(0, 0, 0, 0, 2.7),
In=1.7,
FYM=0,
DR=1.44,
clay=23.4,
xi=1,
solver=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought.}
\item{ks}{A vector of lenght 5 containing the values of the decomposition rates for the different pools}
\item{C0}{A vector of length 5 containing the initial amount of carbon for the 5 pools.}
\item{In}{A scalar or data.frame object specifying the amount of litter inputs by time.}
\item{FYM}{A scalar or data.frame object specifying the amount of Farm Yard Manure inputs by time.}
\item{DR}{A scalar representing the ratio of decomposable plant material to resistant plant material (DPM/RPM).}
\item{clay}{Percent clay in mineral soil.}
\item{xi}{A scalar or data.frame object specifying the external (environmental and/or edaphic) effects on decomposition rates.}
\item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.}
\item{pass}{if TRUE forces the constructor to create the model even if it is invalid}
}
\description{This function implements the RothC model of Jenkinson et al. It is a wrapper for the more general function \code{\link{GeneralModel}}.}
\examples{t=0:500 
Ex=RothCModel(t)
Ct=getC(Ex)
Rt=getReleaseFlux(Ex)

matplot(t,Ct,type="l",col=1:5, ylim=c(0,25),
ylab=expression(paste("Carbon stores (Mg C ", ha^-1,")")),
xlab="Time (years)", lty=1)
lines(t,rowSums(Ct),lwd=2)
legend("topleft",
c("Pool 1, DPM",
"Pool 2, RPM",
"Pool 3, BIO",
"Pool 4, HUM",
"Pool 5, IOM",
"Total Carbon"),
lty=1,
lwd=c(rep(1,5),2),
col=c(1:5,1),
bty="n"
)}
\references{Jenkinson, D. S., S. P. S. Andrew, J. M. Lynch, M. J. Goss, and P. B. Tinker. 1990. The Turnover of Organic Carbon and Nitrogen in Soil. 
Philosophical Transactions: Biological Sciences 329:361-368.
Sierra, C.A., M. Mueller, S.E. Trumbore. 2012. Models of soil organic matter decomposition: the SoilR package version 1.0. Geoscientific Model Development 5, 1045-1060.}
\seealso{\code{\link{ICBMModel}} }
\title{Implementation of the RothCModel}
\value{A Model Object that can be further queried }
