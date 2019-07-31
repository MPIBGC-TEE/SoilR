setMethod(
  f="GeneralDecompOp",
  signature=signature(object="function"),
  def=function 
  (object){
    UnBoundLinDecompOp(object)
  }
)
setMethod(
  f="InFluxes",
  signature=signature(object="function"),
  def=function 
  (object){
    UnBoundInFluxes(object)
  }
)

#' convert a function f of to f_vec
#' 
#' @param obj  For this method a function, whose formal arguments must have names that are elements of the union of poolNames and timeSymbol
#' @param poolNames The ordered poolnames
#' @param timeSymbol The name of the argument of obj that represents time.
#' @return f_vec(vec,t)  A new function that extracts the arguments of obj from a complete vector of state variables and the time argument t and applies the orginal function to these arguments
#' The ode solvers used by SoilR expect a vector valued function of the state vector and time that represents the derivative.
#' The components of this vector are scalar functions of a vector argument and time. It is possible for the  user to define such functions directly, but the definition always depends on the order of state variables. Furthermore these functions usually do not use the complete state vector but only some parts of it.
#' It is much clearer more intuitive and less error prone to be able to define 
#' functions that have only formal arguments that are used. 
#' This is what this method is used for.
#' @examples
#' leaf_resp=function(leaf_pool_content){leaf_pool_content*4}
#' leaf_resp(1)
#' poolNames=c(
#'    "some_thing"
#'   ,"some_thing_else"
#'   ,"some_thing_altogther"
#'   ,"leaf_pool_content"
#' )
#  # create a version of the function f(vec,t) 
#' leaf_resp_vec=by_PoolIndex(leaf_resp,poolNames,timeSymbol='t')
#' # The result is the same since the only the forth position in the vector
#  # is equal to our original leaf_pool_content=1
#' leaf_resp_vec(c(1,27,3,1),5) 

setMethod(
  f="by_PoolIndex",
  signature=signature(
    obj="function"
    ,poolNames='character'
    ,timeSymbol='character'
  ),
  definition=function(obj,poolNames,timeSymbol){
        funcOfVars<-obj
        arg_names<-names(formals(funcOfVars))
        pp('arg_names')
        possibleArgs=c(poolNames,timeSymbol)
        pp('possibleArgs')
        positions<-unlist(lapply(arg_names,function(arg){grep(arg,possibleArgs)}))
        
        vec_func<-function(state_vec,t){
            # append t argument to state vector as time symbol to poolNames
            vec<-c(state_vec,t)
            res<-do.call(
                funcOfVars
                ,as.list((vec[positions]))
            )
            res
        }
        vec_func
  }
)
