\docType{methods}
\name{BoundLinDecompOp,UnBoundLinDecompOp-method}
\title{BoundLinDecompOp,UnBoundLinDecompOp-method 
 convert a UnBoundLinDecompOp to a BoundLinDecompOp}
\alias{BoundLinDecompOp,UnBoundLinDecompOp-method}
\usage{\S4method{BoundLinDecompOp}{UnBoundLinDecompOp}(map,
starttime=-Inf,
endtime=Inf)}
\arguments{
\item{map}{: of class UnBoundLinDecompOp}
\item{starttime}{}
\item{endtime}{}
}
\description{The method creates a BoundLinDecompOp consisting of a constant time dependent function 
and the limits of its domain (starttime and endtime) set to -Inf and Inf respectively}
