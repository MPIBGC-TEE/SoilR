\name{transitTime}
\alias{transitTime}
\usage{transitTime(A= ,
u= ,
a=seq(0, 100),
q=c(0.05, 0.5, 0.95))}
\arguments{
\item{A}{A compartmental linear square matrix with cycling rates in the diagonal and transfer rates in the off-diagonal.}
\item{u}{A one-column matrix defining the amount of inputs per compartment.}
\item{a}{A sequence of ages to calculate density functions}
\item{q}{Vector of probabilities to calculate quantiles of the transit time distribution}
}
\description{Computes the density distribution and mean for the transit time of a compartmental model}
\seealso{\code{\link{systemAge}}}
\title{Transit times for compartment models}
\value{A list with 3 objects: mean transit time, transit time density distribution, and quantiles.}
