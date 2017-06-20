\name{TwopMMmodel}
\alias{TwopMMmodel}
\usage{TwopMMmodel(t,
ks=1.8e-05,
kb=0.007,
Km=900,
r=0.6,
Af=1,
ADD=3.2,
ival)}
\arguments{
\item{t}{vector of times (in days) to calculate a solution.}
\item{ks}{a scalar representing SOM decomposition rate (m3 d-1 (gCB)-1)}
\item{kb}{a scalar representing microbial decay rate (d-1)}
\item{Km}{a scalar representing the Michaelis constant (g m-3)}
\item{r}{a scalar representing the respired carbon fraction (unitless)}
\item{Af}{a scalar representing the Activity factor; i.e. a temperature and moisture modifier (unitless)}
\item{ADD}{a scalar representing the annual C input to the soil (g m-3 d-1)}
\item{ival}{a vector of length 2 with the initial values of the SOM pool and the microbial biomass pool (g m-3)}
}
\title{Implementation of a two-pool Michaelis-Menten model}
\description{This function implements a two-pool Michaelis-Meneten model with a microbial biomass and a substrate pool.}
\details{This implementation is similar to the model described in Manzoni and Porporato (2007). }
\references{Manzoni, S, A. Porporato (2007). A theoretical analysis of nonlinearities and feedbacks in soil carbon and nitrogen cycles.
Soil Biology and Biochemistry 39: 1542-1556.}
\value{An object of class NlModel that can be further queried.}
