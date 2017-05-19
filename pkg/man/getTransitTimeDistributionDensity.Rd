\name{getTransitTimeDistributionDensity}
\alias{getTransitTimeDistributionDensity}
\usage{getTransitTimeDistributionDensity(object,
inputDistribution,
times)}
\arguments{
\item{object}{a protoDecompOp Object}
\item{inputDistribution}{a vector of length equal to the number of pools. The entries are weights. That means that their sume must be equal to one!}
\item{times}{the times for which the distribution density is sought}
}
\title{methods for transit time distributions }
\description{According to  \link{getMeanTransitTime} to we define the related density:}
\references{Manzoni, S., G.G. Katul, and A. Porporato. 2009. Analysis of soil carbon transit times and age distributions using network theories.
Journal of Geophysical Research-Biogeosciences 114, DOI: 10.1029/2009JG001070.}
