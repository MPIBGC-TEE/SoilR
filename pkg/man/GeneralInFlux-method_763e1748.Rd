\docType{methods}
\name{GeneralInFlux,function-method}
\title{GeneralInFlux,function-method 
 creates a UnBoundInFlux from a vector valued function  }
\alias{GeneralInFlux,function-method}
\usage{\S4method{GeneralInFlux}{function}(object)}
\arguments{
\item{object}{: of class function}
}
\description{The resulting operator is created by a call to the constructor of class
UnBoundInFlux. You should only use this if the domain of your function is the complete
time axis (-Inf,+Inf).
If your function has a finite domain create an object of class \code{\link{BoundInFlux-class}}  ### by calling \code{\link{BoundInFlux}}. This will activeate checks on that avoid 
unintended extrapolation.}
