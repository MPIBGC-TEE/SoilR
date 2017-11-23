\description{defines a time dependent inputrate as function of time and including the domain where the function is well defined. This can be used to avoid interpolations out of range when mixing different time dependent data sets}
\name{BoundInFlux-class}
\alias{BoundInFlux-class}
\docType{class}
\section{Methods}{No exported methods directly defined for class BoundInFlux:


Methods inherited from superclasses:

from class InFlux:

  \describe{
    \item{GeneralInFlux}{\code{signature(object = "InFlux")}: ... } \code{\link{GeneralInFlux,InFlux-method}}
	 }
from class TimeMap:

  \describe{
    \item{as.character}{\code{signature(x = "TimeMap")}: ... } \code{\link{as.character,TimeMap-method}}
    \item{GeneralDecompOp}{\code{signature(object = "TimeMap")}: ... } \code{\link{GeneralDecompOp,TimeMap-method}}
    \item{GeneralInFlux}{\code{signature(object = "TimeMap")}: ... } \code{\link{GeneralInFlux,TimeMap-method}}
    \item{getFunctionDefinition}{\code{signature(object = "TimeMap")}: ... } \code{\link{getFunctionDefinition,TimeMap-method}}
    \item{getTimeRange}{\code{signature(object = "TimeMap")}: ... } \code{\link{getTimeRange,TimeMap-method}}
    \item{TimeMap}{\code{signature(map = "TimeMap", starttime = "ANY", endtime = "ANY", times = "ANY", data = "ANY")}: ... } \code{\link{TimeMap,TimeMap,ANY,ANY,ANY,ANY-method}}
	 }}
\title{BoundInFlux S4 class}
\section{Superclasses}{\code{\link{InFlux-class}}\cr
\code{\link{TimeMap-class}}\cr}
\section{Constructors}{	\code{\link{BoundInFlux}}\cr}
