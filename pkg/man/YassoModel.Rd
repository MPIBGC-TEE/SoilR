\name{YassoModel}
\alias{YassoModel}
\usage{YassoModel(t,
ks=c(a_fwl = 0.54,
 a_cwl = 0.03, k_ext = 0.48, k_cel = 0.3, k_lig = 0.22, k_hum1 = 0.012, k_hum2 = 0.0012),
p=c(fwl_ext = 0.1,
 cwl_ext = 0.1,
 fwl_cel = 0.1,
 cwl_cel = 0.1,
 fwl_lig = 0.1, cwl_lig = 0.1, pext = 0.05, pcel = 0.24, plig = 0.77, phum1 = 0.51),
C0,
In=c(u_fwl = 0.0758,
 u_cwl = 0.0866,
 u_nwl_cnwl_ext = 0.251 * 0.3,
 u_nwl_cnwl_cel = 0.251 * 0.3, u_nwl_cnwl_lig = 0.251 * 0.3, 0, 0),
xi=1,
solver=deSolve.lsoda.wrapper,
pass=FALSE)}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought.}
\item{ks}{A vector of lenght 7 containing the values of the exposure and decomposition rates for each pool.}
\item{p}{A vector of containing transfer coefficients among different pools.}
\item{C0}{A vector containing the initial amount of carbon for the 7 pools. The length of this vector must be 7.}
\item{In}{A vector of constatn litter inputs.}
\item{xi}{A scalar or data.frame object specifying the external (environmental and/or edaphic) effects on decomposition rates.}
\item{solver}{A function that solves the system of ODEs. This can be \code{\link{euler}} or \code{\link{ode}} or any other user provided function with the same interface.}
\item{pass}{if TRUE forces the constructor to create the model even if it is invalid}
}
\description{This function creates a model for seven pools as described in Liski et al. (2005). Model not yet implemented due to lack of data in original publication: values of vector p not completely described in paper. 0.1 was assumed. }
\references{Liski, J., Palosuo, T., Peltoniemi, M., and Sievanen, R. (2005).
Carbon and decomposition model Yasso for forest soils. Ecological Modelling, 189:168-182.}
\seealso{\code{\link{ThreepParallelModel}}, \code{\link{ThreepSeriesModel}}}
\title{Implementation of the Yasso model.}
\value{A Model Object that can be further queried }
