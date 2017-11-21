\docType{methods}
\name{getTimeRange,DecompositionOperator-method}
\title{getTimeRange,DecompositionOperator-method 
 ask for the boundaries of the underlying time interval}
\alias{getTimeRange,DecompositionOperator-method}
\usage{\S4method{getTimeRange}{DecompositionOperator}(object= )}
\arguments{
\item{object}{: of class DecompositionOperator}
}
\description{The method returns the time range of the given object 
It is ( probably mostly ) used internally to make sure that 
time dependent functions retrieved from data are not
used outside the interval where they are valid. }
