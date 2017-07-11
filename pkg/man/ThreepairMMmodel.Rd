\name{ThreepairMMmodel}
\alias{ThreepairMMmodel}
\usage{ThreepairMMmodel(t,
ks,
kb,
Km,
r,
Af=1,
ADD,
ival)}
\arguments{
\item{t}{vector of times to calculate a solution.}
\item{ks}{a vector of length 3 representing SOM decomposition rate (m3 d-1 (gCB)-1)}
\item{kb}{a vector of length 3 representing microbial decay rate (d-1)}
\item{Km}{a vector of length 3 representing the Michaelis constant (g m-3)}
\item{r}{a vector of length 3 representing the respired carbon fraction (unitless)}
\item{Af}{a scalar representing the Activity factor; i.e. a temperature and moisture modifier (unitless)}
\item{ADD}{a vector of length 3 representing the annual C input to the soil (g m-3 d-1)}
\item{ival}{a vector of length 6 with the initial values of the SOM pools and the microbial biomass pools (g m-3)}
}
\description{This function implements a 6-pool Michaelis-Meneten model with pairs of microbial biomass and substrate pools.}
\title{Implementation of a 6-pool Michaelis-Menten model}
\value{An object of class NlModel that can be further queried.}
