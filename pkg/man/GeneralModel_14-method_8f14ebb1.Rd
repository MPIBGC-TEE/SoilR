\docType{methods}
\name{GeneralModel_14,numeric,ANY,numeric,ANY,ANY,ANY,missing,numeric,missing,function,logical-method}
\title{GeneralModel_14,numeric,ANY,numeric,ANY,ANY,ANY,missing,numeric,missing,function,logical-method 
 a function to create objects of class Model_14}
\alias{GeneralModel_14,numeric,ANY,numeric,ANY,ANY,ANY,missing,numeric,missing,function,logical-method}
\usage{\S4method{GeneralModel_14}{numeric,ANY,numeric,ANY,ANY,ANY,missing,numeric,missing,function,logical}(t,
A,
ivList,
initialValF,
inputFluxes,
inputFc,
di=-0.0001209681,
solverfunc=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{: of class numeric, A vector containing the points in time where the solution is sought.}
\item{A}{: of class ANY}
\item{ivList}{: of class numeric}
\item{initialValF}{: of class ANY}
\item{inputFluxes}{: of class ANY}
\item{inputFc}{: of class ANY}
\item{di}{: of class numeric}
\item{solverfunc}{: of class function}
\item{pass}{: of class logical}
}
\description{This method tries to create a Model object from any combination of arguments 
that can be converted into  the required set of building blocks for a model
for n arbitrarily connected pools.}
