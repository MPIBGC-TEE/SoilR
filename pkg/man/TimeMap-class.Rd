\description{This class enhances a time dependent function by information about its domain and a possible delay.
The information about the delay is especially usefull for functions that interpolate data. 
Assume that you are give time series data in two vectors \code{times}, \code{values}.
You can create an interpolating function with  \code{\link{splinefun}} or \code{\link{approxfun}}
\code{f  <- splinefun(x=times,y=values) } 
\code{f(t)} will yield sensible values for \eqn{$\min_{t \in times}\le t \le max_{t \in times}$}{min(times)<t<max(times)}.
but will produce unreasonable values for any t outside these limits.
Unfortunately the interpolating functions produced by 
\code{\link{splinefun}} or \code{\link{approxfun}} do not retain any information 
about their domain which makes it possible to accidentatly 
apply them to times not at all supported by the original data. 
This would not even cause errors in the code but silently corrupt the results.
To help you to keep track of the domains of the many time dependent functions used in SoilR's 
Models thss class \code{\link{TimeMap-class}} stores the \code{starttime} and \code{endtime} values
along with the function represented by \code{map}.
SoilR functions that accept time series data will normally convert it to 
subclasses  \code{TimeMap-class} automatically but you can do it explicitly.}
\name{TimeMap-class}
\alias{TimeMap-class}
\docType{class}
\section{Methods}{  \describe{
    \item{as.character}{\code{signature(x = "TimeMap")}: ... } \code{\link{as.character,TimeMap-method}}
    \item{GeneralDecompOp}{\code{signature(object = "TimeMap")}: ... } \code{\link{GeneralDecompOp,TimeMap-method}}
    \item{GeneralInFlux}{\code{signature(object = "TimeMap")}: ... } \code{\link{GeneralInFlux,TimeMap-method}}
    \item{getFunctionDefinition}{\code{signature(object = "TimeMap")}: ... } \code{\link{getFunctionDefinition,TimeMap-method}}
    \item{getTimeRange}{\code{signature(object = "TimeMap")}: ... } \code{\link{getTimeRange,TimeMap-method}}
    \item{TimeMap}{\code{signature(map = "TimeMap", starttime = "ANY", endtime = "ANY", times = "ANY", data = "ANY")}: ... } \code{\link{TimeMap,TimeMap,ANY,ANY,ANY,ANY-method}}
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
