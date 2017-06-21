\docType{methods}
\name{BoundFc,function,numeric,numeric,numeric,character,missing-method}
\title{BoundFc,function,numeric,numeric,numeric,character,missing-method}
\alias{BoundFc,function,numeric,numeric,numeric,character,missing-method}
\usage{\S4method{BoundFc}{function,numeric,numeric,numeric,character,missing}(map,
starttime,
endtime,
lag,
format)}
\arguments{
\item{map}{: of class function, a function of one argument (time)}
\item{starttime}{: of class numeric, the point in time from which map is a valid representation}
\item{endtime}{: of class numeric, the point in time until which map is a valid representation}
\item{lag}{: of class numeric, a scalar describing the time lag. Positive Values shift the argument of the interpolation function forward in time. (retard its effect)}
\item{format}{: of class character, a string that specifies the format used to represent the atmospheric fraction. Possible values are "Delta14C" which is the default or "afn" the Absolute Fraction Normal representation}
}
\description{the method constructs an object from a function a timerange where it is valid and a format  }
