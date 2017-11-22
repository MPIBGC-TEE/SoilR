\docType{methods}
\name{TimeMap,data.frame,missing,missing,missing,missing-method}
\title{TimeMap,data.frame,missing,missing,missing,missing-method 
 constructor}
\alias{TimeMap,data.frame,missing,missing,missing,missing-method}
\usage{\S4method{TimeMap}{data.frame,missing,missing,missing,missing}(map,
lag=0,
interpolation=splinefun)}
\arguments{
\item{map}{: of class data.frame, a data frame containing two columns}
\item{lag}{:  a time delay}
\item{interpolation}{:  the interpolating function}
}
\description{create a  TimeMap object by interpolating 
the data.frame }
