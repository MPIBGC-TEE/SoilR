


#' automatic title
#' 
#' @param map : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
  f="ConstInFluxes",
  signature=c(
    map="numeric"
  ),
  definition=function 
  (
    map
    ){
    new("ConstInFluxes",map=map)
  }
)



#' automatic title
#' 
#' @param map : no manual documentation
#' @param numberOfPools : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    "ConstInFluxes"
    ,signature=signature(
         map='ConstantInFluxList_by_PoolIndex'
        ,numberOfPools='numeric'
    )
    ,def=function(map,numberOfPools){
        res=rep(0,numberOfPools)
        for (f in map){res[[f@destinationIndex]]<-f@flux_constant}
        ConstInFluxes(map=res)
    }
)



#' automatic title
#' 
#' @param object : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    "getConstantInFluxVector"
    ,signature=signature(
         object='ConstInFluxes'
    )
    ,def=function(object){
        object@map
    }
)



#' automatic title
#' 
#' @param object : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f="getTimeRange",
    signature="ConstInFluxes",
    definition=function 
    ( object){
        return(
               c("t_min"=-Inf,"t_max"=Inf))
    }
)



#' automatic title
#' 
#' @param object : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f="getFunctionDefinition",
    signature="ConstInFluxes",
    definition=function(object){
        return(function(t){matrix(ncol=1,object@map)})
    }
)

