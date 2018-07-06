\docType{methods}
\name{BoundFc,character-method}
\title{BoundFc,character-method 
 A constructor}
\alias{BoundFc,character-method}
\usage{\S4method{BoundFc}{character}(format,
...)}
\arguments{
\item{format}{: of class character, a string that specifies the format used to represent the atmospheric fraction. Possible values are "Delta14C" which is the default or "afn" the Absolute Fraction Normal representation}
\item{...}{:  passed on to TimeMap}
}
\description{The method constructs an object from a format string and other parameters which are passed on to
\code{\link{TimeMap}}}
