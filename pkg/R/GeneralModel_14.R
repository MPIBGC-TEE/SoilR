#
# vim:set ff=unix expandtab ts=2 sw=2:
GeneralModel_14  <- function # a function to create objects of class Model_14
  ### This method tries to create a Model object from any combination of arguments 
  ### that can be converted into  the required set of building blocks for a model
  ### for n arbitrarily connected pools.
(t,	##<< A vector containing the points in time where the solution is sought.
 A,	
 ivList,
 initialValF, 
 inputFluxes, 
 Fc,
 inputFc=Fc,
 di=-0.0001209681, 
 solverfunc=deSolve.lsoda.wrapper,
 pass=FALSE  
)
{
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
