\docType{methods}
\name{TimeMap,list,missing,missing,missing,missing-method}
\title{TimeMap,list,missing,missing,missing,missing-method 
 Create a TimeMap from a nested list }
\alias{TimeMap,list,missing,missing,missing,missing-method}
\usage{\S4method{TimeMap}{list,missing,missing,missing,missing}(map)}
\arguments{
\item{map}{: of class list}
}
\description{The method creates an instance of \code{\link{TimeMap-class}}
from a list that contains data and a vector of times referring to it.}
\details{The list must have two entries
If the entries are not named, the first one is supposed to be a numeric vector
of \code{times} and the second to contain the data referring to those times.
The \code{data} entry of the list can itself be a list with the same length as
the \code{times} entry or an array whose last dimension is equal to the length of 
the \code{times} entry.
If the \code{data} entry is a list the elements must 
be \code{vectors},\code{matrices} or \code{arrays}.}
