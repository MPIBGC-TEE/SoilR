\name{fT.Century1}
\alias{fT.Century1}
\usage{fT.Century1(Temp,
Tmax=45,
Topt=35)}
\arguments{
\item{Temp}{A scalar or vector containing values of temperature for which the effects on decomposition rates are calculated.}
\item{Tmax}{A scalar defining the maximum temperature in degrees C.}
\item{Topt}{A scalar defining the optimum temperature for the decomposition process in degrees C.}
}
\title{Effects of temperature on decomposition rates according the the CENTURY model}
\description{Calculates the effects of temperature on decomposition rates according to the CENTURY model.}
\references{Burke, I. C., J. P. Kaye, S. P. Bird, S. A. Hall, R. L. McCulley, and G. L. Sommerville. 2003. 
Evaluating and testing models of terrestrial biogeochemistry: the role of temperature in controlling decomposition. 
Pages 235-253 in C. D. Canham, J. J. Cole, and W. K. Lauenroth, editors. Models in ecosystem science. Princeton University Press, Princeton.}
\value{A scalar or a vector containing the effects of temperature on decomposition rates (unitless).}
