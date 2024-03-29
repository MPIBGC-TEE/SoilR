% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Model_by_PoolNames.R
\name{getTimes,Model_by_PoolNames-method}
\alias{getTimes,Model_by_PoolNames-method}
\title{Extract the times vector}
\usage{
\S4method{getTimes}{Model_by_PoolNames}(object)
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
\description{
Since the \code{times} had to be provided to create the model this method
yields no new information. 
It is usually called internally by other functions that deal with models.
}
