% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Model.R
\name{getAccumulatedRelease,Model-method}
\alias{getAccumulatedRelease,Model-method}
\title{Compute the time integral of the relaese fluxes over time}
\usage{
\S4method{getAccumulatedRelease}{Model}(object)
}
\arguments{
\item{object}{A modelrun as produced by the constructors: 
 \code{\link{Model}},
 \code{\link{Model_by_PoolNames}},
 \code{\link{Model_14}}
the function \code{\link{GeneralModel}} or the functions listed in 
\code{\link{predefinedModels}}. \cr
A model represents the initial value problem (IVP)  
for the contents of the pool consisting of 
\itemize{
 \item{ The initial values of the pool content}
 \item{ The system of ordinary differential equations, as dictated by the fluxes}
 \item{ The times for which the solution of the IVP is evaluated.}
}}
}
\value{
A matrix with as many columns as there are pools 
and as many rows as there are entries in the \code{times} argument the model has been build with.
}
\description{
The definite integral of the vector 
of release fluxes over time from  start to t, computed for all t in the
\code{times} argument the modelrun has been created with.
}
