\docType{methods}
\name{getTimeRange,UnBoundLinDecompOp-method}
\title{getTimeRange,UnBoundLinDecompOp-method 
 return an (infinite) time range since the function is assumed to be valid for all times}
\alias{getTimeRange,UnBoundLinDecompOp-method}
\usage{\S4method{getTimeRange}{UnBoundLinDecompOp}(object= )}
\arguments{
\item{object}{: of class UnBoundLinDecompOp}
}
\description{some functions dealing with DecompOps in general rely on this
so we have to implement it even though the timerange is always the same: (-inf,inf)}
