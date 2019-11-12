


#' automatic title
#' 
#' @param .Object no manual documentation
#' @param starttime no manual documentation
#' @param endtime no manual documentation
#' @param numberOfPools no manual documentation
#' @param alpha no manual documentation
#' @param f no manual documentation
#' @param lag no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f="initialize",
    signature="TransportDecompositionOperator",
    definition=function(
      .Object,
      starttime=numeric(),
      endtime=numeric(),
      numberOfPools=1,
      alpha=list(),
      f=function(t,O){t},
      lag=0
    ){
    .Object@starttime=starttime
    .Object@endtime=endtime
    .Object@numberOfPools=numberOfPools
    .Object@alpha=alpha
    .Object@f=f
    return(.Object)
    }
)
#fromToSplitter=function(){"_to_"}
getRecipient=function(stri){
  as.numeric(unlist(strsplit(stri,split=fromToSplitter()))[[2]])
}
getSender=function(stri){
  as.numeric(unlist(strsplit(stri,split=fromToSplitter()))[[1]])
}



#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getDotOut",
      signature(object="TransportDecompositionOperator"),
      definition=function(object){
      return(object@f)
   }
)



#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getCompartmentalMatrixFunc",
      signature(object="TransportDecompositionOperator"),
      definition=function(object){
          NFunc<-function(C,t){
              outFluxVectorFunc=object@f
              outFluxVec=outFluxVectorFunc(C,t)
              n_vec=as.numeric(outFluxVec/C)
              Nmat <- diag(n_vec)
              Nmat
          }
          TFunc=getTransferMatrixFunc(object)
          BFunc<-function(C,t){
              T <- TFunc(C,t)
              N <- NFunc(C,t)
              B <- T%*%N
              B
          }
      return(BFunc)
   }
)



#' automatic title
#' 
#' @param object no manual documentation
#' @param i no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getOutputReceivers",
   signature(object="TransportDecompositionOperator",i="numeric"),
   definition=function(object,i){
     keys <- names(object@alpha)
     pattern <- paste("^",i,sep="");
     mask <- grepl(pattern,keys)
     js=unlist(lapply(keys[mask],getRecipient))
     return(js)
   }
)
#setMethod(
#   f= "getTransferMatrix",
#   signature(object="TransportDecompositionOperator"),
#   definition=function(object){
#        # fixme mm
#        # add a deprecation warning in the generic
#        return(getTransferMatrixFunc(object))
#   }
#)



#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getTransferMatrixFunc",
      signature(object="TransportDecompositionOperator"),
      definition=function(object){
        alpha=object@alpha
        np=object@numberOfPools
        m=matrix(nrow=np,ncol=np,0)
        for (i in 1:np){m[i,i]=-1}
        keys=names(alpha)
        Tr=function(C,t){
          for (key in keys){
            m[getRecipient(key),getSender(key)]=alpha[[key]](C,t)
          }
          return(m)
        }  
      return(Tr)
   }
)



#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getTransferCoefficients",
      signature(object="TransportDecompositionOperator"),
      definition=function(object){
      return(object@alpha)
   }
)



#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getNumberOfPools",
      signature(object="TransportDecompositionOperator"),
      definition=function(object){
      return(object@numberOfPools)
   }
)



#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f="getFunctionDefinition",
      signature(object="TransportDecompositionOperator"),
      definition=function(object){
      return(object@f)
    }
)
