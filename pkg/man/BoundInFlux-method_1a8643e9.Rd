\docType{methods}
\name{BoundInFlux,ANY,ANY,ANY-method}
\title{BoundInFlux,ANY,ANY,ANY-method}
\alias{BoundInFlux,ANY,ANY,ANY-method}
\usage{\S4method{BoundInFlux}{ANY,ANY,ANY}(map,
starttime,
endtime,
lag=0,
interpolation)}
\arguments{
\item{map}{: of class ANY, see the same argument in  \code{\link{TimeMap}}}
\item{starttime}{: of class ANY, see the same argument in  \code{\link{TimeMap}}}
\item{endtime}{: of class ANY, see the same argument in  \code{\link{TimeMap}}}
\item{lag}{:  see the same argument in  \code{\link{TimeMap}}}
\item{interpolation}{:  see the same argument in  \code{\link{TimeMap}}}
}
\description{The method is used internally to convert TimeMap objects to BoundInFlux objects, since the use of TimeMap objects is now deprecated.}
