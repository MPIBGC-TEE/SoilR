\docType{methods}
\name{getMeanTransitTime,ConstLinDecompOp-method}
\title{getMeanTransitTime,ConstLinDecompOp-method 
 compute the mean transit time }
\alias{getMeanTransitTime,ConstLinDecompOp-method}
\usage{\S4method{getMeanTransitTime}{ConstLinDecompOp}(object,
inputDistribution)}
\arguments{
\item{object}{: of class ConstLinDecompOp}
\item{inputDistribution}{:  distribution of the inputs}
}
\description{This method computes the mean transit time for the linear time invariant system 
that can be constructed from the given operator and input distribution.

It relies on the mehtod \code{getTransitTimeDistributionDensity} using the same arguments.}
\references{Lasaga, A.: The kinetic treatment of geochemical cycles, Geochimica et
Cosmochimica Acta, 44, 815 -- 828, doi{10.1016/0016-7037(80)90263-X}, 1980.}
