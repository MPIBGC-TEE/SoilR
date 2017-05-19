\docType{methods}
\name{BoundInFlux,data.frame,missing,missing,numeric,function-method}
\title{BoundInFlux,data.frame,missing,missing,numeric,function-method}
\alias{BoundInFlux,data.frame,missing,missing,numeric,function-method}
\usage{\S4method{BoundInFlux}{data.frame,missing,missing,numeric,function}(map,
lag,
interpolation)}
\arguments{
\item{map}{: of class data.frame, A data frame; the first column is interpreted as time}
\item{lag}{: of class numeric, lag time}
\item{interpolation}{: of class function, function used for interpolation}
}
\description{This function is another constructor of the class BoundInFlux.}
\value{An object of class BoundInFlux that contains the interpolation function and the limits of the time range where the function is valid. Note that the limits change according to the time lag}
