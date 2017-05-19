\docType{methods}
\name{BoundFc,function,numeric,numeric,missing,character,missing-method}
\title{BoundFc,function,numeric,numeric,missing,character,missing-method}
\alias{BoundFc,function,numeric,numeric,missing,character,missing-method}
\usage{\S4method{BoundFc}{function,numeric,numeric,missing,character,missing}(map,
starttime,
endtime,
format)}
\arguments{
\item{map}{: of class function}
\item{starttime}{: of class numeric}
\item{endtime}{: of class numeric}
\item{format}{: of class character}
}
\description{wrapper with the assumption lag=0}
\value{An object  that contains the interpolation function and the limits of the time range where the function is valid. Note that the limits change according to the time lag}
