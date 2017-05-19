\name{CenturyModel}
\alias{CenturyModel}
\usage{CenturyModel(t,
ks=c(k.STR = 0.094, k.MET = 0.35, k.ACT = 0.14, k.SLW = 0.0038, k.PAS = 0.00013),
C0=c(0, 0, 0, 0, 0),
In,
LN,
Ls,
clay=0.2,
silt=0.45,
xi=1,
solver=deSolve.lsoda.wrapper)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought.}
\item{ks}{A vector of lenght 5 containing the values of the decomposition rates for the different pools. Units in per week.}
\item{C0}{A vector of length 5 containing the initial amount of carbon for the 5 pools.}
\item{In}{A scalar or data.frame object specifying the amount of litter inputs by time (mass per area per week).}
\item{LN}{A scalar representing the lignin to nitrogen ratio of the plant residue inputs.}
\item{Ls}{A scalar representing the fraction of structural material that is lignin.}
\item{clay}{Proportion of clay in mineral soil.}
\item{silt}{Proportion of silt in mineral soil.}
\item{xi}{A scalar or data.frame object specifying the external (environmental and/or edaphic) effects on decomposition rates.}
\item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.}
}
\title{Implementation of the Century model}
\description{This function implements the Century model as described in Parton et al. (1987).}
\references{Parton, W.J, D.S. Schimel, C.V. Cole, and D.S. Ojima. 1987. Analysis of factors controlling soil organic matter levels in Great Plain grasslands.
Soil Science Society of America Journal 51: 1173--1179.
Sierra, C.A., M. Mueller, S.E. Trumbore. 2012. Models of soil organic matter decomposition: the SoilR package version 1.0. Geoscientific Model Development 5, 1045-1060.}
\value{A Model Object that can be further queried }
