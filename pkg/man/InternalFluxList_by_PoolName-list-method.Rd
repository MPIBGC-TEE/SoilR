% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/InternalFluxList_by_PoolName.R
\name{InternalFluxList_by_PoolName,list-method}
\alias{InternalFluxList_by_PoolName,list-method}
\title{constructor from a normal list}
\usage{
\S4method{InternalFluxList_by_PoolName}{list}(object)
}
\arguments{
\item{object}{A list. Either a list of elements of type
\linkS4class{InternalFlux_by_PoolName} or 
a list where the names of the elements are strings of the form
'1->3' (for the flux rate from pool 1 to 2}
}
\value{
An object of class
\linkS4class{ConstantInFluxList_by_PoolIndex} 


The function checks if the elements are of the desired type or can be
converted to it. It is mainly used internally and usually called 
by the front end functions to convert the user supplied arguments.
}
\description{
constructor from a normal list
}
