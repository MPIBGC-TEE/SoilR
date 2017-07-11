\name{ICBMModel}
\alias{ICBMModel}
\usage{ICBMModel(t,
ks=c(k1 = 0.8, k2 = 0.00605),
h=0.13,
r=1.32,
c0=c(Y0 = 0.3, O0 = 3.96),
In=0,
solver=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sougth.}
\item{ks}{A vector of length 2 with the decomposition rates for the young and the old pool.}
\item{h}{Humufication coefficient (transfer rate from young to old pool).}
\item{r}{External (environmental or edaphic) factor.}
\item{c0}{A vector of length 2 with the initial value of carbon stocks in the young and old pool.}
\item{In}{Mean annual carbon input to the soil.}
\item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.}
\item{pass}{if TRUE forces the constructor to create the model even if it is invalid}
}
\description{This function is an implementation of the Introductory Carbon Balance Model (ICBM).
This is simply a two pool model connected in series.}
\references{Andren, O. and T. Katterer. 1997. ICBM: The Introductory Carbon Balance Model 
for Exploration of Soil Carbon Balances. Ecological Applications 7:1226-1236.}
\seealso{\code{\link{TwopSeriesModel}} }
\title{Implementation of the Introductory Carbon Balance Model (ICBM)}
