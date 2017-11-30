\docType{methods}
\name{TimeMap,missing,missing,missing,numeric,array-method}
\title{TimeMap,missing,missing,missing,numeric,array-method 
 Create a TimeMap from times and array }
\alias{TimeMap,missing,missing,missing,numeric,array-method}
\usage{\S4method{TimeMap}{missing,missing,missing,numeric,array}(times,
data,
lag=0,
interpolation=splinefun)}
\arguments{
\item{times}{: of class numeric}
\item{data}{: of class array}
\item{lag}{}
\item{interpolation}{}
}
\description{The method creates an instance of \code{\link{TimeMap-class}}
from a  vector of times and an array whose last dimension   ### is referring to it.}
