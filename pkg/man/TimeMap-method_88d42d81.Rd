\docType{methods}
\name{TimeMap,missing,missing,missing,numeric,list-method}
\title{TimeMap,missing,missing,missing,numeric,list-method 
 Create a TimeMap from a nested list }
\alias{TimeMap,missing,missing,missing,numeric,list-method}
\usage{\S4method{TimeMap}{missing,missing,missing,numeric,list}(times,
data,
lag=0,
interpolation=splinefun)}
\arguments{
\item{times}{: of class numeric, A vector of times}
\item{data}{: of class list, A list of the same length as times with every
list element refering to one time in times}
\item{lag}{:  Either a scalar or an object of the same size as
the list elements describing the time lag of the data}
\item{interpolation}{:  the interpolation method to be used}
}
\description{The method creates an instance of \code{\link{TimeMap-class}}
from a vector of times and a list of the same length, 
containing vectors matrices or arrays }
