setMethod(
    f="UnBoundNonLinDecompOp_by_PoolNames",
    signature=c(
      internal_fluxes='InternalFluxList_by_PoolName'
      ,out_fluxes='OutFluxList_by_PoolName'
      ,timeSymbol='character'
    ),
    definition=function(
      internal_fluxes
      ,out_fluxes
      ,timeSymbol
      ) {
          new(
              'UnBoundNonLinDecompOp_by_PoolNames'
              ,internal_fluxes=internal_fluxes
              ,out_fluxes=out_fluxes
              ,timeSymbol=timeSymbol
          )
      }
)

#' Compartmental Matrix as function of the state vector and time
#' 
#' @param object An object of the class \code{UnBoundNonLinDecompOp_by_PoolNames} which is a representation equivalent to the compartmental matrix
#' but independent of the order of state variables (pools) which therefore 
#' can be translated to any such ordering. 
#' @param timeSymbol The name of the argument representing time in the functions defining the fluxes in \code{object}
#' @param state_variable_names The vector of the names of the state variables.
#' The argument object is a representation of the compartmental system as #' lists of fluxes (internal fluxes and out-fluxes) as functions of the state variables and time. This method translates it to a matrix based formulation specific to a given ordering of the state variables.
#' It is assumed (and checked) that the names formal arguments of the flux functions in \code{object} are a subset of the names of \code{state_variable_names} 
#' The metod is used internally to translate the more intuitive (and more general) flux based description to the matrix based description required by the ode solvers. 
setMethod(
    f="getCompartmentalMatrixFunc"
    ,signature=signature(
        object="UnBoundNonLinDecompOp_by_PoolNames"
        ,timeSymbol='character'
        ,state_variable_names='character'
    )
    ,definition=function(
        object
        ,timeSymbol
        ,state_variable_names
        ){ 
        o_by_Index=UnBoundNonLinDecompOp(
            state_variable_names=state_variable_names
            ,timeSymbol=timeSymbol
            ,operator=object
        )
        o_by_Index@matFunc 
    }
)

#' convert to Indexed version
setMethod(
    f="UnBoundNonLinDecompOp",
    signature=c(
       matFunc="missing"
      ,internal_fluxes='missing'
      ,out_fluxes='missing'
      ,numberOfPools='missing'
      ,state_variable_names="character"
      ,timeSymbol="character"
      ,operator="UnBoundNonLinDecompOp_by_PoolNames"
    ),
    definition=function(state_variable_names,timeSymbol,operator){
      internal_fluxes_by_index=by_PoolIndex(
        obj         =   operator@internal_fluxes
        ,poolNames  =   state_variable_names
        ,timeSymbol =   timeSymbol
      )

      out_fluxes_by_index=by_PoolIndex(
        operator@out_fluxes
        ,poolNames  =   state_variable_names
        ,timeSymbol =   timeSymbol)

      op_by_ind<-UnBoundNonLinDecompOp(
        internal_fluxes =   internal_fluxes_by_index
        ,out_fluxes     =   out_fluxes_by_index
        ,numberOfPools  =   length(state_variable_names)
      )
    }
)
