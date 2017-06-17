\title{TimeMap S4 class}
\description{defines a (time dependent) mapping including the function definition and the ### domain where the function is well defined.  This can be used to avoid interpolations out of range when mixing different time dependent data sets
defines a (time dependent) mapping including the function definition and the ### domain where the function is well defined.  This can be used to avoid interpolations out of range when mixing different time dependent data sets}
\name{TimeMap-class}
\alias{TimeMap-class}
\docType{class}
\section{Methods}{  \describe{
    \item{as.character}{\code{signature(x = "TimeMap")}: ... } \code{\link{as.character,TimeMap-method}}
    \item{BoundInFlux}{\code{signature(map = "TimeMap", starttime = "missing", endtime = "missing", lag = "missing", interpolation = "missing")}: ... } \code{\link{BoundInFlux,TimeMap,missing,missing,missing,missing-method}}
    \item{BoundLinDecompOp}{\code{signature(map = "TimeMap", starttime = "missing", endtime = "missing")}: ... } \code{\link{BoundLinDecompOp,TimeMap,missing,missing-method}}
    \item{DecompOpSubClassInstance}{\code{signature(object = "TimeMap")}: ... } \code{\link{DecompOpSubClassInstance,TimeMap-method}}
    \item{getFunctionDefinition}{\code{signature(object = "TimeMap")}: ... } \code{\link{getFunctionDefinition,TimeMap-method}}
    \item{getTimeRange}{\code{signature(object = "TimeMap")}: ... } \code{\link{getTimeRange,TimeMap-method}}
    \item{InFlux}{\code{signature(object = "TimeMap")}: ... } \code{\link{InFlux,TimeMap-method}}
	 }}
\section{Subclasses}{\describe{
	\code{\link{BoundInFlux-class}}\cr
	\code{\link{TransportDecompositionOperator-class}}\cr
	\code{\link{BoundLinDecompOp-class}}\cr
	\code{\link{BoundFc-class}}\cr
}}
\section{Constructors}{	\code{\link{TimeMap}}\cr}
