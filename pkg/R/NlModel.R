correctnessOfNlModel <- function
(object)
{   
    times=object@times
    Atm=object@DepComp
    ivList=object@initialValues
    InFluxes=object@inputFluxes
    res=TRUE
    tI_min=getTimeRange(InFluxes)["t_min"]
    tI_max=getTimeRange(InFluxes)["t_max"]
    t_min=min(times)
    t_max=max(times)
    if (t_min<tI_min) {
        stop(simpleError("You ordered a timeinterval that starts earlier than the interval your function I(t) (InFluxes) is defined for. \n Have look at the timeMap object of I(t) or the data it is created from")
        )
    }
    if (t_max>tI_max) {
        stop(simpleError("You ordered a timeinterval that ends later than the interval your function I(t) (InFluxes) is defined for. \n Have look at the timeMap object of I(t) or the data it is created from")
        )
    }
    return(res)
}
is.negative=function(number){
   return(number<0)
}
setClass(
   Class="NlModel",
   representation=representation(
        times="numeric"
        ,
        DepComp="TransportDecompositionOperator"
        ,
        initialValues="numeric"
        ,
        inputFluxes="BoundInFluxes"
        ,
        solverfunc="function"
   )
   , validity=correctnessOfNlModel 
)
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
setMethod(
   f= "print",
      signature(x="NlModel"),
      definition=function(x){
   }
)
setMethod(
   f= "getNumberOfPools",
      signature(object="NlModel"),
      definition=function(object){
      return(length(object@initialValues))
   }
)
setMethod(
   f= "show",
      signature(object="NlModel"),
      definition=function(object){
   }
)
setMethod(
   f= "getDecompOp",
      signature= "NlModel",
      definition=function(object){
      return(object@DepComp)
   }
)
setMethod(
   f= "getParticleMonteCarloSimulator",
      signature= "NlModel",
      definition=function(object){
      return(new(Class="MCSim",object))
   }
)
setMethod(
   f= "getTimes",
      signature= "NlModel",
      definition=function(object){
         times=matrix(ncol=1,object@times)
         colnames(times)="times"
      return(times)
   }
)
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
setMethod(
   f= "computeResults",
      signature= "NlModel",
      definition=function(object){
}
)
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
setMethod("$",signature(x="NlModel"), 
        definition=function 
        ( 
          x, 
          name 
        ){
            return(x[name])
        }
)
