\docType{methods}
\name{BoundLinDecompOp,ANY,ANY,ANY-method}
\title{BoundLinDecompOp,ANY,ANY,ANY-method 
 a constructor }
\alias{BoundLinDecompOp,ANY,ANY,ANY-method}
\usage{\S4method{BoundLinDecompOp}{ANY,ANY,ANY}(map,
starttime,
endtime,
lag=0,
interpolation=splinefun)}
\arguments{
\item{map}{: of class ANY, anything that can be used as the \code{map} parameter}
\item{starttime}{: of class ANY, for \code{\link{TimeMap}}}
\item{endtime}{: of class ANY, for \code{\link{TimeMap}}}
\item{lag}{:  a scalar describing the time lag. Positive Values shift the argument of the interpolation function forward in time. (retard its effect)}
\item{interpolation}{:  the interpolation function to be used}
}
\description{Creates a BoundLinDecompOp Object. }
