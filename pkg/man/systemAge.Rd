\name{systemAge}
\alias{systemAge}
\usage{systemAge(A,
u,
a=seq(0, 100),
q=c(0.05, 0.5, 0.95))}
\arguments{
\item{A}{A compartmental linear square matrix with cycling rates in the diagonal and transfer rates in the off-diagonal.}
\item{u}{A one-column matrix defining the amount of inputs per compartment.}
\item{a}{A sequence of ages to calculate density functions}
\item{q}{A vector of probabilities to calculate quantiles of the system age distribution}
}
\description{Computes the density distribution and mean for the system and pool ages of a SoilR model or a matrix representation of a compartmental model}
\seealso{\code{\link{transitTime}}}
\title{System and pool age for compartment models}
\value{A list with 5 objects: mean system age, system age distribution, quantiles of system age distribution, mean pool-age, and pool-age distribution.}
