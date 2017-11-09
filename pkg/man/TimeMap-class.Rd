\description{defines a (time dependent) mapping including the function definition and the ### domain where the function is well defined.  This can be used to avoid interpolations out of range when mixing different time dependent data sets}
\name{TimeMap-class}
\alias{TimeMap-class}
\docType{class}
\section{Methods}{  \describe{
    \item{GeneralDecompOp}{\code{signature(object = "TimeMap")}: ... } \code{\link{GeneralDecompOp,TimeMap-method}}
    \item{GeneralInFlux}{\code{signature(object = "TimeMap")}: ... } \code{\link{GeneralInFlux,TimeMap-method}}
    \item{TimeMap}{\code{signature(map = "TimeMap", starttime = "ANY", endtime = "ANY", times = "ANY", data = "ANY")}: ... } \code{\link{TimeMap,TimeMap,ANY,ANY,ANY,ANY-method}}
    \item{as.character}{\code{signature(x = "TimeMap")}: ... } \code{\link{as.character,TimeMap-method}}
    \item{getFunctionDefinition}{\code{signature(object = "TimeMap")}: ... } \code{\link{getFunctionDefinition,TimeMap-method}}
    \item{getTimeRange}{\code{signature(object = "TimeMap")}: ... } \code{\link{getTimeRange,TimeMap-method}}
	 }}
\title{TimeMap S4 class}
\section{Subclasses}{\describe{
	\code{\link{BoundInFlux-class}}\cr
	\code{\link{TransportDecompositionOperator-class}}\cr
	\code{\link{BoundLinDecompOp-class}}\cr
	\code{\link{BoundFc-class}}\cr
}}
\section{Constructors}{	\code{\link{TimeMap}}\cr
 Please also look at constructors of non virtual subclasses }
