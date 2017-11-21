#
# vim:set ff=unix expandtab ts=2 sw=2:
GeneralModel_14  <- function # a function to create objects of class \linkS4class{Model_14}
  ### At the moment this is just a wrapper for the actual constructor \link{Model_14} 
  ### with additional support for  some now deprecated parameters for backward compatibility.
  ### This role may change in the future to an abstract factory where the actual class of 
  ### the created model will be determined by the supplied parameters.
(t,
 A,	
 ivList,
 initialValF, 
 inputFluxes, 
 Fc=NULL, ##<< deprecated keyword argument, please use inputFc instead
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
  ### A model object that can be further queried. 
  ##seealso<< \code{\link{TwopParallelModel}}, \code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}} 
}
