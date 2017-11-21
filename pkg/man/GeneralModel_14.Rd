\name{GeneralModel_14}
\alias{GeneralModel_14}
\usage{GeneralModel_14(t= ,
A= ,
ivList= ,
initialValF= ,
inputFluxes= ,
Fc=NULL,
inputFc=Fc,
di=-0.0001209681,
solverfunc=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{}
\item{A}{}
\item{ivList}{}
\item{initialValF}{}
\item{inputFluxes}{}
\item{Fc}{deprecated keyword argument, please use inputFc instead}
\item{inputFc}{}
\item{di}{}
\item{solverfunc}{}
\item{pass}{}
}
\description{At the moment this is just a wrapper for the actual constructor \link{Model_14} 
with additional support for  some now deprecated parameters for backward compatibility.
This role may change in the future to an abstract factory where the actual class of 
the created model will be determined by the supplied parameters.}
\seealso{\code{\link{TwopParallelModel}}, \code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}} }
\title{a function to create objects of class \linkS4class{Model_14}}
\value{A model object that can be further queried. }
