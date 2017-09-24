\name{euler}
\alias{euler}
\usage{euler(times,
ydot,
startValues)}
\arguments{
\item{times}{A row vector containing the points in time where the solution is sought.}
\item{ydot}{The function of y and t that computes the derivative for a given point in time and a column vector y.}
\item{startValues}{A column vector with the initial values.}
}
\description{This function can solve arbitrary first order ode systems with the euler forward 
method and an
adaptive time-step size control given a tolerance for the deviation of a coarse and fine 
estimate of the change in y for the next time step.
It is an alternative to \code{\link{deSolve.lsoda.wrapper}} and has the same interface.
It is much slower than ode and should probably be considered less capable in solving stiff ode systems.
However it has one main advantage, which consists in its simplicity.
It is quite easy to see what is going on inside it.
Whenever you don't trust your implementation of another (more efficient but probably also more complex)
ode solver, just compare the result to what this method computes.}
\title{euler}
