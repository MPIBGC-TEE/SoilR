#' constructor from an ordered pair of PoolName (string like) objects and a function with the set of formal argument names forming a 
#' subset of the state_variable_names
#' 
#' @param func A real valued function describing the flux (mass/time)
#' as function of (some of ) the state variables and time.  
#' @param sourceName A string identifying the source pool of the flux
#' @param destinationName A string identifying the destination pool of the flux
setMethod(
  f="InternalFlux_by_PoolName",
  signature=c(
    func='function'
    ,sourceName='character'
    ,destinationName='character'
    ,src_to_dest='missing'
  ),
  def=function(
    func
    ,sourceName
    ,destinationName
    ){
    new(
        'InternalFlux_by_PoolName'
        ,func=func
        ,sourceName=PoolName(sourceName)
        ,destinationName=PoolName(destinationName)
    )
  }
)

#' automatic title
#' 
#' @param func no manual documentation
#' @param src_to_dest no manual documentation
#' @autocomment 
setMethod(
  f="InternalFlux_by_PoolName",
  signature=c(
    func='function'
    ,sourceName='missing'
    ,destinationName='missing'
    ,src_to_dest='character'
  ),
  def=function(
    func
    ,src_to_dest
    ){
      new(
          "InternalFlux_by_PoolName"
          ,sourceName=getSenderName(src_to_dest)
          ,destinationName=getRecipientName(src_to_dest)
          ,func=func
     )
  }
)

#' automatic title
#' 
#' @param obj no manual documentation
#' @param poolNames no manual documentation
#' @param timeSymbol no manual documentation
#' @autocomment 
setMethod(
    f="by_PoolIndex"
    ,signature=signature(
        obj='InternalFlux_by_PoolName'
        ,poolNames='character'
        ,timeSymbol='character'
    )
    ,definition=function(obj,poolNames,timeSymbol){
        fl_by_index<-new(
            "InternalFlux_by_PoolIndex"
            ,sourceIndex=PoolIndex(
                obj@sourceName
                ,poolNames
            )
            ,destinationIndex=PoolIndex(
                obj@destinationName
                ,poolNames
            )
            ,func=by_PoolIndex(
                obj@func
                ,poolNames=poolNames
                ,timeSymbol=timeSymbol
            )
        )
        fl_by_index
    }
)
#' Convert to a numeric value with name of the form 'a->b'
#'
#' @template FluxListAsNumeric
#' @param ... additional arguments
setMethod("as.numeric",
    signature(x = "InternalFlux_by_PoolName"),
    function (x,y,t,time_symbol,...) {
      flux <- x
      num_flux <- apply_to_state_vec_and_time(flux@func,y,t,time_symbol)
      names(num_fluxes) <- src_to_dest_string(
        flux@sourceName,
        flux@destinationName
      )
      num_flux
    }
)
