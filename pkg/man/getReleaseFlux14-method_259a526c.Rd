\docType{methods}
\name{getReleaseFlux14,Model_14-method}
\title{getReleaseFlux14,Model_14-method 
 14C respiration rate for all pools }
\alias{getReleaseFlux14,Model_14-method}
\usage{\S4method{getReleaseFlux14}{Model_14}(object)}
\arguments{
\item{object}{: of class Model_14, the model.}
}
\description{
The function computes the \eqn{^{14}C}{14C} release flux ( mass per time ) for all pools.
Note that the respiration coefficients for \eqn{^{14}C}{14C} 
do not change in comparison to the total C case.
The fraction of \eqn{^{14}C}{14C} lost by respiration 
is not greater for \eqn{^{14}C}{14C} 
although the decay is faster due to the contribution of radioactivity.}
