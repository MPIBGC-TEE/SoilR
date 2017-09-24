\docType{methods}
\name{getC,Model-method}
\title{getC,Model-method}
\alias{getC,Model-method}
\usage{\S4method{getC}{Model}(object)}
\arguments{
\item{object}{: of class Model}
}
\description{This function computes the value for C for each time and pool. }
\value{A matrix with m columns representing the number of pools, and n rows representing the times as specified by the argument
\code{t} in \code{\link{Model}},\code{\link{GeneralModel}} or another model creating function.}
