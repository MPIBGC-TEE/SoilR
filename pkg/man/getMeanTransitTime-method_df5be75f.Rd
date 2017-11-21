\docType{methods}
\name{getMeanTransitTime,ConstLinDecompOp-method}
\title{getMeanTransitTime,ConstLinDecompOp-method 
 compute the mean transit time }
\alias{getMeanTransitTime,ConstLinDecompOp-method}
\usage{\S4method{getMeanTransitTime}{ConstLinDecompOp}(object= ,
inputDistribution= )}
\arguments{
\item{object}{: of class ConstLinDecompOp}
\item{inputDistribution}{:  distribution of the inputs}
}
\description{This method computes the mean transit time for the linear time invariant system 
that can be constructed from the given operator and input distribution.

It relies on the mehtod \code{getTransitTimeDistributionDensity} using the same arguments.}
\details{To compute the mean transit time for the distribution we have to compute the integral
\deqn{
\bar{T} = \int_0^\infty T \cdot S_r\left( \frac{\vec{I}}{I},0,T\right)\; dT
}
for the numerically computed density.
To avoid issues with numerical integration  we dont use \eqn{\infty}{\infty} as upper limit but cut off the integragion interval prematurely.
For this purpose we calculate a maximum response time of the system as \cite{Lasaga}
\deqn{
\tau_{cycle} = \frac{1}{|\min(\lambda_i)|}
}
where \eqn{\lambda_i}{\lambda_i} are non-zero eigenvalues of the matrix 
\eqn{{\bf A}}{{\bf A}}. }
\references{Lasaga, A.: The kinetic treatment of geochemical cycles, Geochimica et
Cosmochimica Acta, 44, 815 -- 828, doi{10.1016/0016-7037(80)90263-X}, 1980.}
