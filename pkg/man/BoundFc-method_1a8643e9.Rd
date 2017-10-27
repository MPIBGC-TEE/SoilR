\docType{methods}
\name{BoundFc,ANY,ANY,ANY-method}
\title{BoundFc,ANY,ANY,ANY-method}
\alias{BoundFc,ANY,ANY,ANY-method}
\usage{\S4method{BoundFc}{ANY,ANY,ANY}(map,
starttime,
endtime,
lag=0,
format,
interpolation=splinefun)}
\arguments{
\item{map}{: of class ANY, anything that can be used as a single \code{map} parameter}
\item{starttime}{: of class ANY, the point in time from which map is a valid representation}
\item{endtime}{: of class ANY, the point in time until which map is a valid representation}
\item{lag}{:  a scalar describing the time lag. Positive Values shift the argument of the interpolation function forward in time. (retard its effect)}
\item{format}{:  a string that specifies the format used to represent the atmospheric fraction. Possible values are "Delta14C" which is the default or "afn" the Absolute Fraction Normal representation}
\item{interpolation}{:  the interpolation function to be used}
}
\description{the method constructs an object from a function a timerange where it is valid and a format  }
