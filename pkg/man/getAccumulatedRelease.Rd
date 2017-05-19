\name{getAccumulatedRelease}
\alias{getAccumulatedRelease}
\usage{getAccumulatedRelease(object)}
\arguments{
\item{object}{A Model object (e.g. of class Model or Model14)
Have a look at the methods for details.}
}
\title{Calculates the accumulated carbon release from the pools as a function of time}
\description{This function computes the accumulated carbon release of the given model as funtion of time. }
\value{A n x m matrix of cummulative release fluxes with m columns representing the number of pools, and n rows representing the times as specified by the argument
\code{t} in \code{\link{GeneralModel}} or other model creating functions.}
