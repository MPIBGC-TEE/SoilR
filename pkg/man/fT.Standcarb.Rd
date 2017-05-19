\name{fT.Standcarb}
\alias{fT.Standcarb}
\usage{fT.Standcarb(Temp,
Topt=45,
Tlag=4,
Tshape=15,
Q10=2)}
\arguments{
\item{Temp}{A scalar or vector containing values of temperature for which the effects on decomposition rates are calculated.}
\item{Topt}{A scalar representing the optimum temperature for decomposition.}
\item{Tlag}{A scalar that determines the lag of the response curve.}
\item{Tshape}{A scalar that determines the shape of the response curve.}
\item{Q10}{A scalar. Temperature coefficient Q10.}
}
\title{Effects of temperature on decomposition rates according to the StandCarb model}
\description{Calculates the effects of temperature on decomposition rates according to the StandCarb model.}
\references{Harmon, M. E., and J. B. Domingo (2001), A users guide to STANDCARB version 2.0: 
A model to simulate carbon stores in forest stands. Oregon State University, Corvallis.}
\value{A scalar or a vector containing the effects of temperature on decomposition rates (unitless).}
