\docType{methods}
\name{TimeMap,data.frame,missing,missing-method}
\title{TimeMap,data.frame,missing,missing-method}
\alias{TimeMap,data.frame,missing,missing-method}
\usage{\S4method{TimeMap}{data.frame,missing,missing}(map,
lag=0,
interpolation=splinefun)}
\arguments{
\item{map}{: of class data.frame, a data frame containing two columns}
\item{lag}{:  a time delay}
\item{interpolation}{:  the interpolating function}
}
\description{create a  TimeMap object by interpolating the data.frame }
\value{An object of class TimeMap that contains the interpolation function and the limits of the time range where the function is valid. Note that the limits change according to the time lag
this serves as a saveguard for Model which thus can check that all involved functions of time are actually defined for the times of interest  }
