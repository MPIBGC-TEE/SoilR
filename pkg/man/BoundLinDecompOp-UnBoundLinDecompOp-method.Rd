% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BoundLinDecompOp.R
\name{BoundLinDecompOp,UnBoundLinDecompOp-method}
\alias{BoundLinDecompOp,UnBoundLinDecompOp-method}
\title{A converter}
\usage{
\S4method{BoundLinDecompOp}{UnBoundLinDecompOp}(map, starttime = -Inf, endtime = Inf)
}
\arguments{
\item{map}{An object of class UnBoundLinDecompOp}

\item{starttime}{Begin of time interval map will be restricted to}

\item{endtime}{End of time interval map will be restricted to}
}
\description{
The distinction between the classes BoundLinDecompOp and UnboundLinDecompOp exist for those functions, that should be only defined for objects of class UnBoundLinDecomp.
 
Many functions however do not need extra methods for objects of class 
UnBoundLinDecompOp and just treat it as a BoundLinDecompOp which is defined
on the complete timeline (-Inf,+Inf). 
With its default arguments this function converts its map argument to a BoundLinDecompOp with just this domain. 
This is the most frequent internal use case. 
If starttime and endtime are provided the domain of the operator will be restricted [starttime,endtime].
}
