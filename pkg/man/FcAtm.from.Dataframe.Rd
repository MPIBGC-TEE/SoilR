\name{FcAtm.from.Dataframe}
\alias{FcAtm.from.Dataframe}
\usage{FcAtm.from.Dataframe(dframe,
lag=0,
interpolation=splinefun,
format)}
\arguments{
\item{dframe}{A data frame containing exactly two columns:
the first one is interpreted as time
the secon one is interpreted as atmospheric C14 fraction in the format mentioned}
\item{lag}{a scalar describing the time lag. Positive Values shift the argument of the interpolation function forward in time. (retard its effect)}
\item{interpolation}{A function that  returns a function  the default is splinefun. Other possible values are the linear interpolation approxfun or any self made function with the same interface.}
\item{format}{a string that specifies the format used to represent the atmospheric fracton. Possible values are "Delta14C" which is the default or "afn" the Absolute Fraction Normal representation}
}
\description{This function is deprecated constructor of the deprecatied class FcAtm}
\title{FcAtm.from.Dataframe}
\value{An object of the new class BoundFc that replaces FcAtm }
