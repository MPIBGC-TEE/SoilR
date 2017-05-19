\name{fT.Century2}
\alias{fT.Century2}
\usage{fT.Century2(Temp,
Tmax=45,
Topt=35)}
\arguments{
\item{Temp}{A scalar or vector containing values of temperature for which the effects on decomposition rates are calculated.}
\item{Tmax}{A scalar defining the maximum temperature in degrees C.}
\item{Topt}{A scalar defining the optimum temperature for the decomposition process in degrees C.}
}
\title{Effects of temperature on decomposition rates according the the CENTURY model}
\description{Calculates the effects of temperature on decomposition rates according to the CENTURY model.}
\references{Adair, E. C., W. J. Parton, S. J. D. Grosso, W. L. Silver, M. E. Harmon, S. A. Hall, I. C. Burke, and S. C. Hart. 2008. 
Simple three-pool model accurately describes patterns of long-term litter decomposition in diverse climates. Global Change Biology 14:2636-2660.}
\value{A scalar or a vector containing the effects of temperature on decomposition rates (unitless).}
