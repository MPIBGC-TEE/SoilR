#' additional function to create Models
#' 
#' In previous SoilR Version GeneralModel was the function to create linear
#' models, a task now fulfilled by the function \code{\link{Model}}. To ensure
#' backward compatibility this function remains as a wrapper. In future
#' versions it might take on the role of an abstract factory that produces
#' several classes of models (i.e autonomous or non-autonomous and linear or non-linear) depending on different
#' combinations of arguments. It creates a Model object from any combination of
#' arguments that can be converted into the required set of building blocks for
#' a model for n arbitrarily connected pools.
#' 
#' 
#' @param t A vector containing the points in time where the solution is
#' sought.
#' @param A Anything that can be converted by \link{GeneralDecompOp} to any of
#' the available DecompositionOperator classes
#' @param ivList A vector containing the initial amount of carbon for the n
#' pools. The length of this vector is equal to the number of pools and thus
#' equal to the length of k. This is checked by an internal function.
#' @param inputFluxes something that can be converted to any of the available
#' InFluxes classes
#' @param solverfunc The function used by to actually solve the ODE system.
#' This can be \code{\link{deSolve.lsoda.wrapper}} or any other user provided
#' function with the same interface.
#' @param pass Forces the constructor to create the model even if it is invalid
#' @param timeSymbol A string (character vector of lenght 1) identifying the variable name 
#  for time in the function definitions for the fluxes
#' @return A model object that can be further queried.
#' @seealso \code{\link{TwopParallelModel}}, \code{\link{TwopSeriesModel}},
#' \code{\link{TwopFeedbackModel}}
GeneralModel <- function(
    t			
    ,A			
    ,ivList		
    ,inputFluxes 
    ,solverfunc=deSolve.lsoda.wrapper		
    ,pass=FALSE  
    ,timeSymbol
){  
     if (
         # this if-clause is a prelimnary solution. 
         # At the latest when we have components of other types 
         # from which Model_by_PoolNames instance can be created
         # we should first convert it to the allowed superclasses
         # if possible.
         is(A,"UnBoundNonLinDecompOp_by_PoolNames") && is(inputFluxes,"InFluxList_by_PoolName")
     ){
        return(
            Model_by_PoolNames(
                times=t
                ,mat=A
                ,initialValues=ivList
                ,inputFluxes=inputFluxes
                ,solverfunc
                ,pass
                ,timeSymbol
            )
        )
     }else{
        return(Model(t,A,ivList,inputFluxes,solverfunc,pass))
    }
}
