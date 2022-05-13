is.negative=function(number){
   return(number<0)
}



#' automatic title
#' 
#' @param .Object no manual documentation
#' @param times no manual documentation
#' @param DepComp no manual documentation
#' @param initialValues no manual documentation
#' @param inputFluxes no manual documentation
#' @param solverfunc no manual documentation
#' @param pass no manual documentation
#' @autocomment
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
         if (inherits(inputFluxes, "TimeMap")){
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



#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment
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



#' automatic title
#' 
#' @param x no manual documentation
#' @autocomment
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



#' automatic title
#' 
#' @param x no manual documentation
#' @autocomment
setMethod(
   f= "print",
      signature(x="NlModel"),
      definition=function(x){
   }
)



#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment
setMethod(
   f= "getNumberOfPools",
      signature(object="NlModel"),
      definition=function(object){
      return(length(object@initialValues))
   }
)



#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment
setMethod(
   f= "show",
      signature(object="NlModel"),
      definition=function(object){
   }
)



#' Extract the Compartmental Operator
#' 
#' @template Model-param
#' @template getDecompOp-description-common
#' @autocomment 
setMethod(
   f= "getDecompOp",
      signature= "NlModel",
      definition=function(object){
      return(object@DepComp)
   }
)



#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment
setMethod(
   f= "getParticleMonteCarloSimulator",
      signature= "NlModel",
      definition=function(object){
      return(new(Class="MCSim",object))
   }
)



#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment
setMethod(
   f= "getTimes",
      signature= "NlModel",
      definition=function(object){
         times=matrix(ncol=1,object@times)
         colnames(times)="times"
      return(times)
   }
)



#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment
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



#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment
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



#' automatic title
#' 
#' @param object no manual documentation
#' @param as.closures no manual documentation
#' @autocomment
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



#' automatic title
#' 
#' @param object no manual documentation
#' @param as.closures no manual documentation
#' @autocomment
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



#' automatic title
#' 
#' @param object no manual documentation
#' @autocomment
setMethod(
   f= "getReleaseFlux",
      signature= "NlModel",
      definition=function 
      (
      object 
      ){
      DecOp=object@DepComp
      DotO=getDotOut(DecOp) 
      Tr=getTransferMatrixFunc(DecOp)
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





#' Pool Contents for all times
#' 
#' @template getC-description-common
#' @template Model-param
#' @param as.closures If \code{TRUE} will return the result as a list of
#' approximating functions of time indexed by the pool number.
#' @param object no manual documentation
#' @return 
#' If \code{as.closures} is \code{FALSE} (the default) the return value is a matrix with as many columns as there are pools 
#' and as many rows as there are entries in the \code{times} 
#' argument the model has been built with.
#' @autocomment 
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
      Tr=getTransferMatrixFunc(DepComp) 
      force(DotO) 
      itm=object@inputFluxes
      inputrates=getFunctionDefinition(itm)
      DotC=function(C,t){
        return(Tr(C,t)%*%DotO(C,t)+inputrates(t))
      }
      sVmat=matrix(object@initialValues,ncol=1)

      # the solver will give back a matrix with as many columns as pools
      # and as many rows as times
      C_values=solver(times,DotC,sVmat,object@solverfunc) 
      prec=1e-5
      pos=which(C_values< -prec,arr.ind=T)
      timesOfNegativeCStocks=times[pos[,1]]
      if(length(timesOfNegativeCStocks)>0){
        errorPlotC(C_values,times)
        stop("negative CStocks")
      }
      n <- nrow(C_values)
      # create a list of row vectors one for each time step
      ktvec_list <- mclapply(
        1:length(times),
        function(i){
          C_t<- C_values[i,]
          t <- times[[i]]
          DotO(C_t,t)/C_t
        }
      )
      # tranform the list into a matrix like the C/values
      kt_values=matrix(
        ncol=ncol(C_values),
        byrow=TRUE,
        unlist(ktvec_list)
      )
      # in this matrix we can now look 

      pos=which(kt_values< -prec,arr.ind=T)
      timesOfSOMCreation=times[pos[,1]]
      if(length(timesOfSOMCreation)>0){
        print(timesOfSOMCreation)
        errorPlotC(C_values,times)
        stop("SOM creation")
      }
      if (as.closures){
        return(res2fun(times,C_values))
      }else{
        return(C_values)
      }
   }
)



#' automatic title
#' 
#' @param x no manual documentation
#' @param i no manual documentation
#' @autocomment
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



#' automatic title
#' 
#' @param x no manual documentation
#' @param name no manual documentation
#' @autocomment
setMethod("$",signature(x="NlModel"), 
        definition=function 
        ( 
          x, 
          name 
        ){
            return(x[name])
        }
)
