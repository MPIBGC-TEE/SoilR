\docType{methods}
\name{getF14R,Model_14-method}
\title{getF14R,Model_14-method 
 average radiocarbon fraction weighted by carbonrelease }
\alias{getF14R,Model_14-method}
\usage{\S4method{getF14R}{Model_14}(object)}
\arguments{
\item{object}{: of class Model_14, an object}
}
\description{Calculates the average radiocarbon fraction weighted by the amount of carbon release at each time step. 
\eqn{\overline{F_R}=\frac{\sum_{i=1}^{n}{^{14}R_i}}{\sum_{i=1}^{n}{R_i}}}{(14R_1(t)+...+14R_n(t)) )/(R_1(t)+...R_n(t)))}
Where \eqn{^{14}R_i(t)}{14R_i(t)} is the time dependent release of \eqn{^{14}C}{14C} of pool \eqn{i} and \eqn{R_i(t)}{R_i(t)} the release of all carbon isotops of pool \eqn{i}.   
Since the result is always in Absolute Fraction Modern format wie have to convert it to Delta14C}
