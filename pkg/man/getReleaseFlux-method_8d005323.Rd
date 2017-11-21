\docType{methods}
\name{getReleaseFlux,Model-method}
\title{getReleaseFlux,Model-method 
 get the release rate for all pools }
\alias{getReleaseFlux,Model-method}
\usage{\S4method{getReleaseFlux}{Model}(object= )}
\arguments{
\item{object}{: of class Model, an object of class Model
created by a call to a constructor e.g. \code{\link{Model}}, 
\code{\link{GeneralModel}}or other model creating functions.}
}
\description{The method computes the release of carbon per time for all points in time 
specified in the Model objects time slot.}
\details{This function takes a Model object, which represents a system of ODEs 
\deqn{\frac{d \mathbf{C}(t)}{dt} = \mathbf{I}(t) + \mathbf{A}(t) \mathbf{C}(t)}{dC(t)/dt = I(t) + A(t)C(t)} 
solves the system for \eqn{\mathbf{C}(t)}{C(t)}, calculates the release coefficients \eqn{\mathbf{R}(t)}{R(t)}, 
and computes the release flux as \eqn{\mathbf{R}(t) \mathbf{C}(t)}{R(t) C(t)}.
The numerical solver used can be specified in the model creating functions like e.g. \code{\link{Model}}.}
\value{A n x m matrix of release fluxes with m columns representing the number of pools, and n rows representing the values for each time step as specified by the argument
\code{t} in \code{\link{Model}}, \code{\link{GeneralModel}} or another model creating function.}
