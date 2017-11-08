\docType{methods}
\name{Model_14,numeric,ANY,numeric,ConstFc,ANY,ANY,numeric,missing,logical-method}
\title{Model_14,numeric,ANY,numeric,ConstFc,ANY,ANY,numeric,missing,logical-method 
 constructor for class Model_14}
\alias{Model_14,numeric,ANY,numeric,ConstFc,ANY,ANY,numeric,missing,logical-method}
\usage{\S4method{Model_14}{numeric,ANY,numeric,ConstFc,ANY,ANY,numeric,missing,logical}(t,
A,
ivList,
initialValF,
inputFluxes,
inputFc,
c14DecayRate,
pass=FALSE)}
\arguments{
\item{t}{: of class numeric}
\item{A}{: of class ANY}
\item{ivList}{: of class numeric}
\item{initialValF}{: of class ConstFc}
\item{inputFluxes}{: of class ANY}
\item{inputFc}{: of class ANY}
\item{c14DecayRate}{: of class numeric}
\item{pass}{: of class logical}
}
\description{This method is a wrapper 
with solverfunc set to \code{deSolve.lsoda.wrapper}}
\seealso{\code{\link{TwopParallelModel14}}, \code{\link{TwopSeriesModel14}}, \code{\link{TwopFeedbackModel14}} and so on. }
