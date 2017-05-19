\name{GaudinskiModel14}
\alias{GaudinskiModel14}
\usage{GaudinskiModel14(t,
ks=c(kr = 1/1.5,
 koi = 1/1.5, koeal = 1/4, koeah = 1/80, kA1 = 1/3, kA2 = 1/75, kM = 1/110),
C0=c(FR0 = 390, C10 = 220, C20 = 390, C30 = 1370, C40 = 90, C50 = 1800, C60 = 560),
F0_Delta14C=rep(0, 7),
LI=150,
RI=255,
xi=1,
inputFc,
lambda=-0.0001209681,
lag=0,
solver=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought. It must be specified within the same period for which the Delta 14 C of the atmosphere is provided. The default period in the provided dataset \code{\link{C14Atm_NH}} is 1900-2010.}
\item{ks}{A vector of length 7 containing the decomposition rates for the 6 soil pools plus the fine-root pool.}
\item{C0}{A vector of length 7 containing the initial amount of carbon for the 6 pools plus the fine-root pool.}
\item{F0_Delta14C}{A vector of length 7 containing the initial amount of the radiocarbon fraction for the 7 pools as Delta14C values in per mil.}
\item{LI}{A scalar or a data.frame object specifying the amount of litter inputs by time.}
\item{RI}{A scalar or a data.frame object specifying the amount of root inputs by time.}
\item{xi}{A scalar or a data.frame specifying the external (environmental and/or edaphic) effects on decomposition rates.}
\item{inputFc}{A Data Frame object containing values of atmospheric Delta14C per time. First column must be time values, second column must be Delta14C values in per mil.}
\item{lambda}{Radioactive decay constant. By default lambda=-0.0001209681 y^-1 . This has the side effect that all your time related data are treated as if the time unit was year.}
\item{lag}{A positive integer representing a time lag for radiocarbon to enter the system.}
\item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or  any other user provided function with the same interface.}
\item{pass}{if TRUE Forces the constructor to create the model even if it is invalid}
}
\title{Implementation of a the six-pool C14 model proposed by Gaudinski et al. 2000}
\description{This function creates a model as described in Gaudinski et al. 2000. 
It is a wrapper for the more general functions  \code{\link{GeneralModel_14}} that can handle an arbitrary number of pools.}
\references{Gaudinski JB, Trumbore SE, Davidson EA, Zheng S (2000) Soil carbon cycling in a temperate forest: radiocarbon-based estimates of residence times, sequestration rates and partitioning fluxes. Biogeochemistry 51: 33-69}
\value{A Model Object that can be further queried }
