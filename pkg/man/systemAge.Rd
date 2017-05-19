\name{systemAge}
\alias{systemAge}
\usage{systemAge(A,
u,
a=seq(0, 100))}
\arguments{
\item{A}{A compartmental linear square matrix with cycling rates in the diagonal and transfer rates in the off-diagonal.}
\item{u}{A one-column matrix defining the amount of inputs per compartment.}
\item{a}{A sequence of ages to calculate density functions}
}
\title{System and pool age for compartment models}
\description{Computes the density distribution and mean for the system and pool ages of a SoilR model or a matrix representation of a compartmental model}
\value{A list with 4 objects: mean system age, system age distribution, mean pool-age, and pool-age distribution.}
