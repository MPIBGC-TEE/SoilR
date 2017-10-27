\docType{methods}
\name{BoundFc,ANY,missing,missing-method}
\title{BoundFc,ANY,missing,missing-method}
\alias{BoundFc,ANY,missing,missing-method}
\usage{\S4method{BoundFc}{ANY,missing,missing}(map,
lag=0,
format,
interpolation=splinefun)}
\arguments{
\item{map}{: of class ANY, anything that can be used as a single \code{map} parameter}
\item{lag}{:  a scalar describing the time lag. Positive Values shift the argument of the interpolation function forward in time. (retard its effect)}
\item{format}{:  a string that specifies the format used to represent the atmospheric fraction. Possible values are "Delta14C" which is the default or "afn" the Absolute Fraction Normal representation}
\item{interpolation}{:  the interpolation function to be used}
}
\description{the method constructs an object from a function a timerange where it is valid and a format  }
