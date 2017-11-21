\docType{methods}
\name{getTimeRange,UnBoundInFlux-method}
\title{getTimeRange,UnBoundInFlux-method 
 return an (infinite) time range since the function is assumed to be valid for all times}
\alias{getTimeRange,UnBoundInFlux-method}
\usage{\S4method{getTimeRange}{UnBoundInFlux}(object= )}
\arguments{
\item{object}{: of class UnBoundInFlux}
}
\description{some functions dealing with DecompOps in general rely on this
so we have to implement it even though the timerange is always the same: (-inf,inf)}
