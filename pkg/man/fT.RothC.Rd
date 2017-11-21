\name{fT.RothC}
\alias{fT.RothC}
\usage{fT.RothC(Temp= )}
\arguments{
\item{Temp}{A scalar or vector containing values of temperature for which the effects on decomposition rates are calculated.}
}
\description{Calculates the effects of temperature on decomposition rates according to the functions included in the RothC model.}
\note{This function returns NA for Temp <= -18.3}
\references{Jenkinson, D. S., S. P. S. Andrew, J. M. Lynch, M. J. Goss, and P. B. Tinker (1990), 
The Turnover of Organic Carbon and Nitrogen in Soil, Philosophical Transactions: Biological Sciences, 329(1255), 361-368.}
\title{Effects of temperature on decomposition rates according to the functions included in the RothC model}
\value{A scalar or a vector containing the effects of temperature on decomposition rates (unitless).}
