\name{getC}
\alias{getC}
\usage{getC(object,
as.closures=F)}
\arguments{
\item{object}{some model object, the actual class depends on the method used.}
\item{as.closures}{if set to TRUE instead of a matrix a list of functions will be returned.}
}
\title{Calculates the C content of the pools }
\description{This function computes the carbon content of the pools as function of time.
Have a look at the methods for details.}
\value{A matrix with m columns representing the number of pools, and n rows representing the times as specified by the argument
\code{t} in \code{\link{GeneralModel}} or another model creating function.}
