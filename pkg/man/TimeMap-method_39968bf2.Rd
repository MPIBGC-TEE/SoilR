\docType{methods}
\name{TimeMap,missing,missing,missing,numeric,matrix-method}
\title{TimeMap,missing,missing,missing,numeric,matrix-method 
 Create a TimeMap from a nested list }
\alias{TimeMap,missing,missing,missing,numeric,matrix-method}
\usage{\S4method{TimeMap}{missing,missing,missing,numeric,matrix}(times,
data,
lag=0,
interpolation=splinefun)}
\arguments{
\item{times}{: of class numeric}
\item{data}{: of class matrix, a matrix, every column corresponds to one time step}
\item{lag}{:  a scalar or a vector describing how much the data is laging behind the times}
\item{interpolation}{:  the interpolation method to be used}
}
\description{The method creates an instance of \code{\link{TimeMap-class}}
from a  vector of times and an array referring to it.}
