\name{getMeanTransitTime}
\alias{getMeanTransitTime}
\usage{getMeanTransitTime(object,
inputDistribution)}
\arguments{
\item{object}{a DecompOp Object.}
\item{inputDistribution}{a vector of length equal to the number of pools. The entries are weights, which must sum to 1.}
}
\section{Methods}{	\code{\link{getMeanTransitTime,ConstLinDecompOp-method}}\cr}
\title{Access to the mean transit time}
\description{This generic function assembles methods to 
compute mean transit times.
The nature of the results can change considerably 
depending on the arguments of the function.
For an argument of class \code{model} 
it means something different than for an object of class 
\code{DecompOp}
To interpret them correctly refer also to the documentation
of the methods.}
\references{Manzoni, S., G.G. Katul, and A. Porporato. 2009. Analysis of soil carbon transit times and age distributions using network theories.
Journal of Geophysical Research-Biogeosciences 114, DOI: 10.1029/2009JG001070.

Thompson, M.~V. and Randerson, J.~T.: Impulse response functions of terrestrial
carbon cycle models: method and application, Global Change Biology, 5,
371--394, 10.1046/j.1365-2486.1999.00235.x, 1999.

Bolin, B. and Rodhe, H.: A note on the concepts of age distribution and transit
time in natural reservoirs, Tellus, 25, 58--62, 1973.

Eriksson, E.: Compartment Models and Reservoir Theory, Annual Review of Ecology
and Systematics, 2, 67--84, 1971.}
