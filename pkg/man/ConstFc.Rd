\name{ConstFc}
\alias{ConstFc}
\usage{ConstFc(values=c(0),
format=Delta14C)}
\arguments{
\item{values}{a numeric vector}
\item{format}{a character string describing the format e.g. "Delta14C"}
}
\title{creates an object containing the initial values for the 14C fraction needed to create models in SoilR}
\description{The function returns an object of class ConstFc which is a building block for any 14C model in SoilR.
The building blocks of a model have to keep iformation about the formats their data are in, because the high level function dealing wiht the models have to know. This function is actually a convienient wrapper for a call to R's standard constructor new, to hide its complexity from the user.}
\value{An object of class ConstFc that contains data and a format description that can later be used to convert the data into other formats if the conversion is implemented.}
