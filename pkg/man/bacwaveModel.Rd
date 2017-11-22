\name{bacwaveModel}
\alias{bacwaveModel}
\usage{bacwaveModel(t,
umax=0.063,
ks=3,
theta=0.23,
Dmax=0.26,
kd=14.5,
kr=0.4,
Y=0.44,
ival=c(S0 = 0.5, X0 = 1.5),
BGF=0.15,
ExuM=8,
ExuT=0.8)}
\examples{hours=seq(0,800,0.1)
#
#Run the model with default parameter values
bcmodel=bacwaveModel(t=hours)
Cpools=getC(bcmodel)
#
#Time solution
matplot(hours,Cpools,type="l",ylab="Concentrations",xlab="Hours",lty=1,ylim=c(0,max(Cpools)*1.2))
legend("topleft",c("Substrate", "Microbial biomass"),lty=1,col=c(1,2),bty="n")
#
#State-space diagram
plot(Cpools[,2],Cpools[,1],type="l",ylab="Substrate",xlab="Microbial biomass")
#
#Microbial biomass over time
plot(hours,Cpools[,2],type="l",col=2,xlab="Hours",ylab="Microbial biomass")}
\arguments{
\item{t}{vector of times (in hours) to calculate a solution.}
\item{umax}{a scalar representing the maximum relative growth rate of bacteria (hr-1)}
\item{ks}{a scalar representing the substrate constant for growth (ug C /ml soil solution)}
\item{theta}{a scalar representing soil water content (ml solution/cm3 soil)}
\item{Dmax}{a scalar representing the maximal relative death rate of bacteria (hr-1)}
\item{kd}{a scalar representing the substrate constant for death of bacteria (ug C/ml soil solution)}
\item{kr}{a scalar representing the fraction of death biomass recycling to substrate (unitless)}
\item{Y}{a scalar representing the yield coefficient for bacteria (ug C/ugC)}
\item{ival}{a vector of length 2 with the initial values for the substrate and the bacterial pools (ug C/cm3)}
\item{BGF}{a scalar representing the constant background flux of substrate (ug C/cm3 soil/hr)}
\item{ExuM}{a scalar representing the maximal exudation rate (ug C/(hr cm3 soil))}
\item{ExuT}{a scalar representing the  time constant for exudation, responsible for duration of exudation (1/hr).}
}
\description{This function implements the microbial model Bacwave (bacterial waves), a two-pool model with a bacterial and a substrate pool. It is a special case of the general nonlinear model.}
\details{This implementation containts default parameters presented in Zelenev et al. (2000). It produces nonlinear damped oscillations in the form of a stable focus.}
\references{Zelenev, V.V., A.H.C. van Bruggen, A.M. Semenov. 2000. ``BACWAVE,'' a spatial-temporal model for traveling waves of bacterial populations
in response to a moving carbon source in soil. Microbail Ecology 40: 260-272.}
\seealso{There are other \code{\link{predefinedModels}} and also more general functions like \code{\link{Model}}.}
\title{Implementation of the microbial model Bacwave (bacterial waves)}
\value{An object of class NlModel that can be further queried.}
