\name{GeneralModel_14}
\alias{GeneralModel_14}
\usage{GeneralModel_14(t,
A,
ivList,
initialValF,
inputFluxes,
Fc,
di=-0.0001209681,
solverfunc=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought.}
\item{A}{}
\item{ivList}{}
\item{initialValF}{}
\item{inputFluxes}{}
\item{Fc}{}
\item{di}{}
\item{solverfunc}{}
\item{pass}{}
}
\description{This method tries to create a Model object from any combination of arguments 
that can be converted into  the required set of building blocks for a model
for n arbitrarily connected pools.}
\seealso{\code{\link{TwopParallelModel}}, \code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}} }
\title{a function to create objects of class Model_14}
\value{A model object that can be further queried. }
