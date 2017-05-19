\name{transitTime}
\alias{transitTime}
\usage{transitTime(A,
u,
a=seq(0, 100))}
\arguments{
\item{A}{A compartmental linear square matrix with cycling rates in the diagonal and transfer rates in the off-diagonal.}
\item{u}{A one-column matrix defining the amount of inputs per compartment.}
\item{a}{A sequence of ages to calculate density functions}
}
\title{Transit times for compartment models}
\description{Computes the density distribution and mean for the transit time of a compartmental model}
\value{A list with 2 objects: mean transit time, and transit time density distribution.}
