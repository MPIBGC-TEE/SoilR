\description{This class enhances a time dependent function by information about its domain.
The information about the delay is especially usefull for functions that interpolate data. 
Assume that you are given time series data in two vectors \code{times}, \code{values}.
You can create an interpolating function with  \code{\link{splinefun}} or \code{\link{approxfun}}
\code{f  <- splinefun(x=times,y=values) } 
\code{f(t)} will yield sensible values for \eqn{\min_{t \in times}\le t \le max_{t \in times}.}{min(times)<t<max(times).} 
but will produce unreasonable values for any t outside these limits.
Unfortunately the interpolating functions produced by 
\code{\link{splinefun}} or \code{\link{approxfun}} do not retain any information 
about their domain which makes it possible to accidentatly 
apply them to times not at all supported by the original data. 
This would not even cause errors in the code but silently corrupt the results.
To help you to keep track of the domains of the many time dependent functions used in SoilR's 
Models this class \code{\linkS4class{TimeMap}} stores the \code{starttime} 
and \code{endtime} values
along with the function represented by \code{map}.
SoilR functions that accept time series data will normally convert it to 
subclasses  \code{TimeMap-class} automatically but you can do it explicitly.}
\name{TimeMap-class}
\alias{TimeMap-class}
\docType{class}
\section{Methods}{Exported methods directly defined for class TimeMap:

  \describe{
    \item{GeneralDecompOp}{\code{signature(object = "TimeMap")}: ... } \code{\link{GeneralDecompOp,TimeMap-method}}
    \item{GeneralInFlux}{\code{signature(object = "TimeMap")}: ... } \code{\link{GeneralInFlux,TimeMap-method}}
    \item{TimeMap}{\code{signature(map = "TimeMap", starttime = "ANY", endtime = "ANY", times = "ANY", data = "ANY")}: ... } \code{\link{TimeMap,TimeMap,ANY,ANY,ANY,ANY-method}}
    \item{add_plot}{\code{signature(x = "TimeMap")}: ... } \code{\link{add_plot,TimeMap-method}}
    \item{as.character}{\code{signature(x = "TimeMap")}: ... } \code{\link{as.character,TimeMap-method}}
    \item{getFunctionDefinition}{\code{signature(object = "TimeMap")}: ... } \code{\link{getFunctionDefinition,TimeMap-method}}
    \item{getTimeRange}{\code{signature(object = "TimeMap")}: ... } \code{\link{getTimeRange,TimeMap-method}}
	 }}
\title{TimeMap S4 class}
\section{Subclasses}{\describe{
	\code{\linkS4class{BoundInFlux}}\cr
	\code{\linkS4class{TransportDecompositionOperator}}\cr
	\code{\linkS4class{BoundLinDecompOp}}\cr
	\code{\linkS4class{BoundFc}}\cr
}}
\section{Constructors}{	\code{\link{TimeMap}}\cr
 Please also look at constructors of non virtual subclasses: 
\code{\link{BoundInFlux}}, , \code{\link{BoundLinDecompOp}}, \code{\link{BoundFc}}.}
