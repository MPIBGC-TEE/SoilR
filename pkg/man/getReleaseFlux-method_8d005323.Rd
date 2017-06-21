\docType{methods}
\name{getReleaseFlux,Model-method}
\title{getReleaseFlux,Model-method}
\alias{getReleaseFlux,Model-method}
\usage{\S4method{getReleaseFlux}{Model}(object)}
\arguments{
\item{object}{: of class Model, an object of class Model
created by a call to a constructor e.g. \code{\link{Model}}, 
\code{\link{GeneralModel}}or other model creating functions.}
}
\description{The method computes the release of carbon per time for all points in time 
specified in the Model objects time slot.}
\value{A n x m matrix of release fluxes with m columns representing the number of pools, and n rows representing the time step as specified by the argument
\code{t} in \code{\link{Model}}, \code{\link{GeneralModel}} or another model creating function.}
