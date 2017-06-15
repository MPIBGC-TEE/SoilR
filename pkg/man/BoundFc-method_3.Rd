\docType{methods}
\name{BoundFc,data.frame,missing,missing,numeric,character,function-method}
\title{BoundFc,data.frame,missing,missing,numeric,character,function-method}
\alias{BoundFc,data.frame,missing,missing,numeric,character,function-method}
\usage{\S4method{BoundFc}{data.frame,missing,missing,numeric,character,function}(map,
lag=0,
format,
interpolation=splinefun)}
\arguments{
\item{map}{: of class data.frame, A data frame containing exactly two columns:
the first one is interpreted as time
the second one is interpreted as atmospheric C14 fraction in the format mentioned}
\item{lag}{: of class numeric, a scalar describing the time lag. Positive Values shift the argument of the interpolation function forward in time. (retard its effect)}
\item{format}{: of class character, a string that specifies the format used to represent the atmospheric fraction. Possible values are "Delta14C" which is the default or "afn" the Absolute Fraction Normal representation}
\item{interpolation}{: of class function, A function that  returns a function  the default is splinefun. Other possible values are the linear interpolation approxfun or any self made function with the same interface.}
}
\description{the method constructs an object from a dataframe a timelag format using the given interpolating function}
