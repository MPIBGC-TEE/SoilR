is.negative=function(number){
   return(number<0)
}



#' Automatic description: initialize,NlModel-method
#' 
#' @name initialize,NlModel-method
#' @param .Object : object of class:\code{NlModel}, no manual documentation
#' @param times : no manual documentation
#' @param DepComp : no manual documentation
#' @param initialValues : no manual documentation
#' @param inputFluxes : no manual documentation
#' @param solverfunc : no manual documentation
#' @param pass : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
    f="initialize",
    signature="NlModel",
    definition=function(
        .Object,
        times=c(0,1),
        DepComp=new(Class="TransportDecompositionOperator",
                0,
                1,
                function(t){
                    return(matrix(nrow=1,ncol=1,0))
                },
                function(t){
                    return(matrix(nrow=1,ncol=1,0))
                }
        ) 
        ,
        initialValues=numeric()
        ,
        inputFluxes= BoundInFluxes(
            function(t){
                return(matrix(nrow=1,ncol=1,1))
            },
            0,
            1
        )
        ,
        solverfunc=deSolve.lsoda.wrapper
        ,
        pass=FALSE
        ){
        .Object@times=times
        .Object@DepComp=DepComp
         if (class(inputFluxes)=="TimeMap"){
          warning(
            "The use of object of class TimeMap for the inputFlux argument is deprecated.
            At the moment we cast TimeMap objects to the new class BoundInFluxes
            which replaces TimeMap as class of the the inputFlux argument.
            To get rid of this warning adapt your code to use a BoundInFluxes instead of a TimeMap.
            Other classes may be implemented in the future." 
            )
            inputFluxes<- BoundInFluxes(inputFluxes)
         }
        .Object@initialValues=initialValues
        .Object@inputFluxes=inputFluxes
        .Object@solverfunc=solverfunc
        if (pass==FALSE) correctnessOfNlModel(.Object) 
        return(.Object)
    }
)



#' Automatic description: getInFluxes,NlModel-method
#' 
#' @name getInFluxes,NlModel-method
#' @param object : object of class:\code{NlModel}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getInFluxes",
   signature(object="NlModel"),
   definition=function(object){
       return(object@inputFluxes)
     }
)
errorPlotC=function(C,t){
      timePlot=function(x,...){
        scaledTime=t/max(t)*max(x)
        lines(scaledTime,col="red",x)
      }
      if(ncol(C)>1){
        pairs(C,diag.panel=timePlot)
      }
}



#' Automatic description: plot,NlModel-method
#' 
#' @name plot,NlModel-method
#' @param x : object of class:\code{NlModel}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "plot",
      signature(x="NlModel"),
      definition=function(x){
      C=getC(x)
      t=getTimes(x)
      cul=getCumulativeC(x)
      errorPlotC(C,t)
      plot(t,cul)
   }
)



#' Automatic description: print,NlModel-method
#' 
#' @name print,NlModel-method
#' @param x : object of class:\code{NlModel}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "print",
      signature(x="NlModel"),
      definition=function(x){
   }
)



#' Automatic description: getNumberOfPools,NlModel-method
#' 
#' @name getNumberOfPools,NlModel-method
#' @param object : object of class:\code{NlModel}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getNumberOfPools",
      signature(object="NlModel"),
      definition=function(object){
      return(length(object@initialValues))
   }
)



#' Automatic description: show,NlModel-method
#' 
#' @name show,NlModel-method
#' @param object : object of class:\code{NlModel}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "show",
      signature(object="NlModel"),
      definition=function(object){
   }
)



#' Automatic description: getDecompOp,NlModel-method
#' 
#' @name getDecompOp,NlModel-method
#' @param object : object of class:\code{NlModel}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getDecompOp",
      signature= "NlModel",
      definition=function(object){
      return(object@DepComp)
   }
)



#' Automatic description: getParticleMonteCarloSimulator,NlModel-method
#' 
#' @name getParticleMonteCarloSimulator,NlModel-method
#' @param object : object of class:\code{NlModel}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getParticleMonteCarloSimulator",
      signature= "NlModel",
      definition=function(object){
      return(new(Class="MCSim",object))
   }
)



#' Automatic description: getTimes,NlModel-method
#' 
#' @name getTimes,NlModel-method
#' @param object : object of class:\code{NlModel}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getTimes",
      signature= "NlModel",
      definition=function(object){
         times=matrix(ncol=1,object@times)
         colnames(times)="times"
      return(times)
   }
)



#' Automatic description: getCumulativeC,NlModel-method
#' 
#' @name getCumulativeC,NlModel-method
#' @param object : object of class:\code{NlModel}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getCumulativeC",
      signature= "NlModel",
      definition=function(object){
         Cpools=getC(object)
         cul=matrix(nrow=nrow(Cpools),0)
         for (i in 1:ncol(Cpools)){cul=cul+Cpools[,i]}
      return(cul)
   }
)



#' Automatic description: getInitialValues,NlModel-method
#' 
#' @name getInitialValues,NlModel-method
#' @param object : object of class:\code{NlModel}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getInitialValues",
      signature= "NlModel",
      definition=function(object){
      return(object@initialValues)
   }
)
res2fun=function(times,C){
  cn=colnames(C)
  if (length(cn)==0){cn=1:nrow(C)}
  Cs=list()
  for (i in 1:ncol(C)){
    key=cn[[i]]
    Cs[[key]] <- splinefun(times,C[,i])
  }
  return(Cs)
}



#' Automatic description: getTransferCoefficients,NlModel-method
#' 
#' @name getTransferCoefficients,NlModel-method
#' @param object : object of class:\code{NlModel}, no manual documentation
#' @param as.closures : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getTransferCoefficients",
      signature= "NlModel",
      definition=function 
      (object,as.closures=F){
      C=getC(object)
      t=object@times
      DepComp=object@DepComp
      alpha=getTransferCoefficients(DepComp)
      if(length(names(alpha))==1){
        all_tr=matrix(ncol=1,mapply(alpha[[1]],(1:length(t))))
        colnames(all_tr)=names(alpha)
      }else{
        single_tr=function(i){
          l=list()
          for (key in names(alpha)){
            force(key)
            l[[key]]=alpha[[key]](C[i,],t[[i]])
          }
          return(l)
        }
        all_tr=t(mapply(single_tr,(1:length(t))))
      }
      if (as.closures){
        return(res2fun(t,all_tr))
      }else{
        return(all_tr)
      }
   }
)



#' Automatic description: getOutputFluxes,NlModel-method
#' 
#' @name getOutputFluxes,NlModel-method
#' @param object : object of class:\code{NlModel}, no manual documentation
#' @param as.closures : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getOutputFluxes",
      signature= "NlModel",
      definition=function 
      (object,as.closures=F){
      C=getC(object)
      t=getTimes(object)
      DepComp=object@DepComp
      DotO=getDotOut(DepComp) 
      single_O=function(i){DotO(C[i,],t[[i]])}
      all_O=t(mapply(single_O,(1:length(t))))
      if (as.closures){
        return(res2fun(t,all_O))
      }else{
        return(all_O)
      }
   }
)



#' Automatic description: getReleaseFlux,NlModel-method
#' 
#' @name getReleaseFlux,NlModel-method
#' @param object : object of class:\code{NlModel}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getReleaseFlux",
      signature= "NlModel",
      definition=function 
      (
      object 
      ){
      DecOp=object@DepComp
      DotO=getDotOut(DecOp) 
      Tr=getTransferMatrix(DecOp)
      C=getC(object)
      nc=ncol(C)
      t=getTimes(object)
      r_t=function(Tr_t){
        values<-mclapply(1:nc,function(j){s<- -sum(Tr_t[,j])})
        m<-matrix(nrow=nc,ncol=1,data=as.numeric(values))
        return(m)
      }
      singleRes=function(i){
        r_t(Tr(C[i,],t[i]))*DotO(C[i,],t[i])
      }
      allRes=t(mcmapply(singleRes,(1:length(t))))
      return(allRes)
   }
)



#' Automatic description: computeResults,NlModel-method
#' 
#' @name computeResults,NlModel-method
#' @param object : object of class:\code{NlModel}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "computeResults",
      signature= "NlModel",
      definition=function(object){
}
)



#' Automatic description: getC,NlModel-method
#' 
#' @name getC,NlModel-method
#' @param object : object of class:\code{NlModel}, no manual documentation
#' @param as.closures : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
   f= "getC",
      signature= c("NlModel"),
      definition=function 
      (
        object,           
        as.closures=FALSE 
      ){
      times=object@times
      DepComp=object@DepComp
      DotO=getDotOut(DepComp) 
      Tr=getTransferMatrix(DepComp) 
      force(DotO) 
      itm=object@inputFluxes
      inputrates=getFunctionDefinition(itm)
      DotC=function(C,t){
        return(Tr(C,t)%*%DotO(C,t)+inputrates(t))
      }
      sVmat=matrix(object@initialValues,ncol=1)
      C=solver(times,DotC,sVmat,object@solverfunc) 
      prec=1e-5
      pos=which(C< -prec,arr.ind=T)
      timesOfNegativeCStocks=times[pos[,1]]
      if(length(timesOfNegativeCStocks)>0){
        errorPlotC(C,times)
        stop("negative CStocks")
      }
      n=nrow(C)
      kts=matrix(nrow=n,mcmapply(FUN=function(C,t){DotO(C,t)/C},C,times)) 
      pos=which(kts< -prec,arr.ind=T)
      timesOfSOMCreation=times[pos[,1]]
      if(length(timesOfSOMCreation)>0){
        print(timesOfSOMCreation)
        errorPlotC(C,times)
        stop("SOM creation")
      }
      if (as.closures){
        return(res2fun(times,C))
      }else{
        return(C)
      }
   }
)



#' Automatic description: [,NlModel,character,ANY,ANY-method
#' 
#' @name [,NlModel,character,ANY,ANY-method
#' @param x : object of class:\code{NlModel}, no manual documentation
#' @param i : object of class:\code{character}, no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod(
  f="[",
  signature(x="NlModel",i="character"),
  definition=function 
  (
    x, 
    i  
  ){
      getSingleCol=function(slot_name){
          res=""
          if(slot_name=="times"){ res=getTimes(x)}
          if(slot_name=="C"){ res=getC(x)}
          return(res)
      }
      n=length(i)
      df=getSingleCol(i[1])
      if (n>1){
          for (k in 2:n){
              df=cbind(df,getSingleCol(i[k]))
          }
      }
      return(df)
  }
)



#' Automatic description: $,NlModel-method
#' 
#' @name $,NlModel-method
#' @param x : object of class:\code{NlModel}, no manual documentation
#' @param name : no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
setMethod("$",signature(x="NlModel"), 
        definition=function 
        ( 
          x, 
          name 
        ){
            return(x[name])
        }
)
