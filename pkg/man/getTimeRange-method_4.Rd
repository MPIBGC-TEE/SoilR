\docType{methods}
\name{getTimeRange,TimeMap-method}
\title{getTimeRange,TimeMap-method}
\alias{getTimeRange,TimeMap-method}
\usage{\S4method{getTimeRange}{TimeMap}(object)}
\arguments{
\item{object}{: of class TimeMap, An object of class TimeMap or one that inherits from TimeMap}
}
\description{The method returns the time range of the given object 
It is probably mostly used internally to make sure that 
time dependent functions retrieved from data are not
used outside the interval where they are valid. }
\value{a vector of length two \code{ c(t_min,t_max) }
containing start and end time of the time interval 
for which the TimeMap object has been defined.}
