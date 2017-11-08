\docType{methods}
\name{GeneralModel_14,numeric,ANY,numeric,ANY,ANY,missing,ANY,numeric,missing,missing,missing-method}
\title{GeneralModel_14,numeric,ANY,numeric,ANY,ANY,missing,ANY,numeric,missing,missing,missing-method 
 a constructor for class Model}
\alias{GeneralModel_14,numeric,ANY,numeric,ANY,ANY,missing,ANY,numeric,missing,missing,missing-method}
\usage{\S4method{GeneralModel_14}{numeric,ANY,numeric,ANY,ANY,missing,ANY,numeric,missing,missing,missing}(t,
A,
ivList,
initialValF,
inputFluxes,
Fc,
di=-0.0001209681,
pass=FALSE)}
\arguments{
\item{t}{: of class numeric, A vector containing the points in time where the solution is sought.}
\item{A}{: of class ANY}
\item{ivList}{: of class numeric}
\item{initialValF}{: of class ANY}
\item{inputFluxes}{: of class ANY}
\item{Fc}{: of class ANY}
\item{di}{: of class numeric}
\item{pass}{: of class missing}
}
\description{A wrapper for \code{\link{Model_14}} with pass=FALSE}
\seealso{\code{\link{TwopParallelModel}}, \code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}} }
