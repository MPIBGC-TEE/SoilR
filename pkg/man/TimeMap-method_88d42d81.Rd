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
\item{times}{: of class numeric}
\item{data}{: of class list}
\item{lag}{}
\item{interpolation}{}
}
\description{The method creates an instance of \code{\link{TimeMap-class}}
from a list that contains data and a vector of times referring to it.}
