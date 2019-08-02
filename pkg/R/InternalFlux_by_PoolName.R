setClass(
  Class="InternalFlux_by_PoolName",
  # contains="PoolConnection", we do not want to iherit mehtods...
  slots=c(sourceName='PoolName',destinationName='PoolName',func='function')
)

#' constructor from an ordered pair of PoolName (string like) objects and a function with the set of formal argument names forming a 
#' subset of the state_variable_names
#' 
#' @param time_symbol A string idicating which of the formal arguments of \code{map} will be considered as time. 
#' @param state_variable_ames A character vector with the
#' names of the variables. If present the vector will be used together with 
#' the map argument. In this case only names in this vector and the time_symbol 
#' argument are permitted as formal arguments of map.
#' @param map A real valued function describing the flux (mass/time)#' as function of the state variables and time.  
#' If \code{state_variables_names} is given  
#' the names of the formal arguments of \code{map}.
#" This is the preferred way
#' SoilR will then use this imformation to derive a function of one 
#' vector argument for the state vector and time as required by
#' the solvers
#' If \code{state_variable_names} is missing 
#' 
#" This allows SoilR to compute a version of the function that takes 
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
                ,timeSymbol=timeSymbol
                ,poolNames=poolNames
            )
        )
        fl_by_index
    }
)
