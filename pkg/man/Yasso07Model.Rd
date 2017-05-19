\name{Yasso07Model}
\alias{Yasso07Model}
\usage{Yasso07Model(t,
ks=c(kA = 0.66, kW = 4.3, kE = 0.35, kN = 0.22, kH = 0.0033),
p=c(p1 = 0.32,
 p2 = 0.01,
 p3 = 0.93,
 p4 = 0.34,
 p5 = 0,
 p6 = 0,
 p7 = 0.035, p8 = 0.005, p9 = 0.01, p10 = 5e-04, p11 = 0.03, p12 = 0.92, pH = 0.04),
C0,
In,
xi=1,
solver=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought.}
\item{ks}{A vector of lenght 5 containing the values of the decomposition rates for each pool.}
\item{p}{A vector of length 13 containing transfer coefficients among different pools.}
\item{C0}{A vector containing the initial amount of carbon for the 5 pools. The length of this vector must be 5.}
\item{In}{A single scalar or data.frame object specifying the amount of litter inputs by time}
\item{xi}{A scalar or data.frame object specifying the external (environmental and/or edaphic) effects on decomposition rates.}
\item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.}
\item{pass}{if TRUE forces the constructor to create the model even if it is invalid}
}
\title{Implementation of the Yasso07 model}
\description{This function creates a model for five pools as described in Tuomi et al. (2009)}
\references{Tuomi, M., Thum, T., Jarvinen, H., Fronzek, S., Berg, B., Harmon, M., Trofymow, J., Sevanto, S., and Liski, J. (2009).
Leaf litter decomposition-estimates of global variability based on Yasso07 model. Ecological Modelling, 220:3362 - 3371.}
\value{A Model Object that can be further queried }
