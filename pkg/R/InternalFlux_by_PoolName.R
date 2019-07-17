#setClass(
#  Class="InternalFlux_by_PoolName",
#  # contains="PoolConnection", we do not want to iherit mehtods...
#  slots=c(sourceId='PoolId',destinationId='PoolId',func='function')
#)
#
##setMethod(
##  f="InternalFlux_by_PoolName_by_PoolIndex",
##  signature=c(map='function'),
##  def=function(map,...){
##    pc<-InternalFlux_by_PoolName_by_PoolIndex(...)
##    intFl=as(pc,'InternalFlux_by_PoolName_by_PoolIndex')
##    intFl@func<-map
##    return(intFl)
##  }
##)
#
##' constructor from an ordered pair of PoolName (string like) objects and a function with the set of formal argument names forming a 
##' subset of the state_variable_names
##' 
##' @param time_symbol A string idicating which of the formal arguments of \code{map} will be considered as time. 
##' @param state_variable_ames A character vector with the
##' names of the variables. If present the vector will be used together with 
##' the map argument. In this case only names in this vector and the time_symbol 
##' argument are permitted as formal arguments of map.
##' @param map A real valued function describing the flux (mass/time)#' as function of the state variables and time.  
##' If \code{state_variables_names} is given  
##' the names of the formal arguments of \code{map}.
##" This is the preferred way
##' SoilR will then use this imformation to derive a function of one 
##' vector argument for the state vector and time as required by
##' the solvers
##' If \code{state_variable_names} is missing 
##' 
##" This allows SoilR to compute a version of the function that takes 
#setMethod(
#  f="InternalFlux_by_PoolName",
#  signature=c(src_to_dest='missing'),
#  def=function(source,destination){
#    new(
#        'InternalFlux_by_PoolName'
#        ,sourceId=GeneralPoolId(source)
#        ,destinationId=GeneralPoolId(destination)
#    )
#  }
#)
