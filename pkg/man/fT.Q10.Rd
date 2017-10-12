\name{fT.Q10}
\alias{fT.Q10}
\usage{fT.Q10(Temp,
k_ref=1,
T_ref=10,
Q10=2)}
\arguments{
\item{Temp}{A scalar or vector containing values of temperature for which the effects on decomposition rates are calculated.}
\item{k_ref}{A scalar representing the value of the decomposition rate at a reference temperature vaule.}
\item{T_ref}{A scalar representing the reference temperature.}
\item{Q10}{A scalar. Temperature coefficient Q10.}
}
\description{Calculates the effects of temperature on decomposition rates according to the modified Van't Hoff function (Q10 function).}
\title{Effects of temperature on decomposition rates according to a Q10 function}
\value{A scalar or a vector containing the effects of temperature on decomposition rates (unitless).}
