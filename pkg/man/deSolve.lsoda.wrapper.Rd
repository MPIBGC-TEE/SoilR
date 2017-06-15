\name{deSolve.lsoda.wrapper}
\alias{deSolve.lsoda.wrapper}
\usage{deSolve.lsoda.wrapper(t,
ydot,
startValues)}
\arguments{
\item{t}{A row vector containing the points in time where the solution is sought.}
\item{ydot}{The function of y and t that computes the derivative for a given
point in time and a column vector y.}
\item{startValues}{A column vector with the starting values.}
}
\title{deSolve.lsoda.wrapper}
\description{The function serves as a wrapper for lsoda using a much simpler interface which allows the use 
of matrices in the definition of the derivative. 
To use lsoda we have to convert our vectors to lists, define tolerances and so on.
This function does this for us , so we don't need to bother about it.}
\value{A matrix. Every column represents a pool and every row a point in time}
