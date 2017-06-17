\name{getReleaseFlux}
\alias{getReleaseFlux}
\usage{getReleaseFlux(object)}
\arguments{
\item{object}{An model object (the actual class depends on the method e.g. Model or  Model14}
}
\section{Methods}{	\code{\link{getReleaseFlux,Model-method}}\cr
	\code{\link{getReleaseFlux,NlModel-method}}\cr}
\title{Calculates the release of C from each pool}
\description{This function computes carbon release from each pool of the given model as funtion of time 
Have a look at the methods for details.}
\value{A matrix. Every column represents a pool and every row a point in time}
