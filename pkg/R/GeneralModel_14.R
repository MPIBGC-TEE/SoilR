#' create objects of class \linkS4class{Model_14}
#' 
#' At the moment this is just a wrapper for the actual constructor
#' \link{Model_14} with additional support for some now deprecated parameters
#' for backward compatibility. This role may change in the future to an
#' abstract factory where the actual class of the created model will be
#' determined by the supplied parameters.
#' 
#' 
#' @param t A vector containing the points in time where the solution is
#' sought.
#' @param A something that can be converted by \link{GeneralDecompOp} to any of
#' the available subclasses of \code{\linkS4class{DecompOp}}.
#' @param ivList A vector containing the initial amount of carbon for the n
#' pools. The length of this vector is equal to the number of pools and thus
#' equal to the length of k. This is checked by an internal function.
#' @param initialValF An object equal or equivalent to class ConstFc containing
#' a vector with the initial values of the radiocarbon fraction for each pool
#' and a format string describing in which format the values are given.
#' @param inputFluxes something that can be converted by \link{InFluxes}
#' to any of the available subclasses of \linkS4class{InFluxes}.
#' @param Fc deprecated keyword argument, please use inputFc instead
#' @param inputFc An object describing the fraction of C_14 in per mille
#' (different formats are possible)
#' @param di the rate at which C_14 decays radioactively. If you don't provide a
#' value here we assume the following value: k=-0.0001209681 y^-1 . This has
#' the side effect that all your time related data are treated as if the time
#' unit was year. Thus beside time itself it also affects decay rates the
#' inputrates and the output
#' @param solverfunc The function used by to actually solve the ODE system.
#' This can be \code{\link{deSolve.lsoda.wrapper}} or any other user provided
#' function with the same interface.
#' @param pass Forces the constructor to create the model even if it is invalid
#' @return A model object that can be further queried.
#' @seealso \code{\link{TwopParallelModel}}, \code{\link{TwopSeriesModel}},
#' \code{\link{TwopFeedbackModel}}
GeneralModel_14  <- function 
(t,
 A,	
 ivList,
 initialValF, 
 inputFluxes, 
 Fc=NULL, 
 inputFc=Fc,
 di=-0.0001209681, 
 solverfunc=deSolve.lsoda.wrapper,
 pass=FALSE  
)
{
  if (!is.null(Fc)){warning("The parameter Fc has been renamed to inputFc. The use of Fc is deprecated. Please change your code accordingly to stay compatible with future versions of the package.")}
  obj=Model_14(
    t=t,
    A=A,
    ivList=ivList,
    initialValF=initialValF, 
    inputFluxes=inputFluxes, 
    inputFc=inputFc,
    c14DecayRate=di,
    solverfunc=solverfunc,
    pass=pass
  )
  return(obj)
}
