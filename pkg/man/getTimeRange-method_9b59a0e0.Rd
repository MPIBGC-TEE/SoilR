\docType{methods}
\name{getTimeRange,ConstInFlux-method}
\title{getTimeRange,ConstInFlux-method 
 time domain of the function}
\alias{getTimeRange,ConstInFlux-method}
\usage{\S4method{getTimeRange}{ConstInFlux}(object)}
\arguments{
\item{object}{: of class ConstInFlux}
}
\description{The method returns a vector containing the start and end time where the intepolation is valid. Since the class \code{\link{ConstInFlux}} represents an input stream
constant in time it will return -infinity,+infinity}
