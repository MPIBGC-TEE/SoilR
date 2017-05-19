\docType{methods}
\name{BoundInFlux,data.frame,missing,missing,missing,missing-method}
\title{BoundInFlux,data.frame,missing,missing,missing,missing-method}
\alias{BoundInFlux,data.frame,missing,missing,missing,missing-method}
\usage{\S4method{BoundInFlux}{data.frame,missing,missing,missing,missing}(map)}
\arguments{
\item{map}{: of class data.frame, A data frame; the first column is interpreted as time}
}
\description{This function is another constructor of the class BoundInFlux.}
\value{An object of class BoundInFlux that contains the interpolation function and the limits of the time range where the function is valid. Note that the limits change according to the time lag}
