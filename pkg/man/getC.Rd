\description{no Description}
\value{A matrix with m columns representing the number of pools, and n rows representing the times as specified by the argument
\code{t} in \code{\link{GeneralModel}} or another model creating function.}
\name{getC}
\alias{getC}
\usage{getC(object,
as.closures=F)}
\title{getC S4 generic}
\arguments{
\item{object}{some model object, the actual class depends on the method used.}
\item{as.closures}{if set to TRUE instead of a matrix a list of functions will be returned.}
}
\section{Methods}{	\code{\link{getC,Model-method}}\cr
	\code{\link{getC,NlModel-method}}\cr}
