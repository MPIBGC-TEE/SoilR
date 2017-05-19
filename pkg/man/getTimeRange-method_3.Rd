\docType{methods}
\name{getTimeRange,DecompositionOperator-method}
\title{getTimeRange,DecompositionOperator-method}
\alias{getTimeRange,DecompositionOperator-method}
\usage{\S4method{getTimeRange}{DecompositionOperator}(object)}
\arguments{
\item{object}{: of class DecompositionOperator}
}
\description{The method returns the time range of the given object 
It is ( probably mostly ) used internally to make sure that 
time dependent functions retrieved from data are not
used outside the interval where they are valid. }
\value{a vector of length two \code{ c(t_min,t_max) }
containing start and end time of the time interval 
for which the object has been defined.}
