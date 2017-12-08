\docType{methods}
\name{TimeMap,missing,missing,missing,numeric,numeric-method}
\title{TimeMap,missing,missing,missing,numeric,numeric-method 
 Create a TimeMap from a nested list }
\alias{TimeMap,missing,missing,missing,numeric,numeric-method}
\usage{\S4method{TimeMap}{missing,missing,missing,numeric,numeric}(times,
data,
lag=0,
interpolation=splinefun)}
\arguments{
\item{times}{: of class numeric}
\item{data}{: of class numeric}
\item{lag}{:  a scalar or a vector,matrix or array describing how much
the data lag behind the times.
If lag is a vector, matrix or array, the resulting          ## function will be vector, matrix or array valued accordingly.}
\item{interpolation}{:  the interpolation method to be used}
}
\description{The method creates an instance of \code{\link{TimeMap-class}}
from a  vector of times and an array referring to it.}
