\docType{methods}
\name{getTimeRange,ConstLinDecompOp-method}
\title{getTimeRange,ConstLinDecompOp-method 
 return an (infinite) time range since the operator is constant}
\alias{getTimeRange,ConstLinDecompOp-method}
\usage{\S4method{getTimeRange}{ConstLinDecompOp}(object)}
\arguments{
\item{object}{: of class ConstLinDecompOp}
}
\description{some functions dealing with DecompOps in general rely on this
so we have to implement it even though the timerange is always the same: (-inf,inf)}
