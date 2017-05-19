#
# vim:set ff=unix expandtab ts=2 sw=2:
correctnessOfNlModel=function
### The parameters used have a biological meaning, and therefore cannot be arbitrary.
### This functions tests some of the obvious constraints of SOM (Soil Organinc Matter) decomposition models. 
### Up to now these are:
### 1) The inputrates that must be positive for the whole time 
### 
### 2) The compatibility of the time ranges of the supplied functions which is important 

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
    #print("tests passed")
    return(res)
}
is.negative=function(number){
   ### the function returns True if the argumente is negative
   return(number<0)
}
### serves as a fence to the interface of SoilR functions. So that later implementations can differ	 
setClass(# NlModel
   Class="NlModel",
   representation=representation(
        times="numeric"
        ,
        DepComp="TransportDecompositionOperator"
        ,
        initialValues="numeric"
        ,
        inputFluxes="BoundInFlux"
        ,
        solverfunc="function"
   )
   , validity=correctnessOfNlModel #set the validating function
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
        inputFluxes= BoundInFlux(
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
       # cat("-initializer at work-\n")
        .Object@times=times
        .Object@DepComp=DepComp
         if (class(inputFluxes)=="TimeMap"){
          warning(
            "The use of object of class TimeMap for the inputFlux argument is deprecated.
            At the moment we cast TimeMap objects to the new class BoundInFlux
            which replaces TimeMap as class of the the inputFlux argument.
            To get rid of this warning adapt your code to use a BoundInFlux instead of a TimeMap.
            Other classes may be implemented in the future." 
            )
            # cast
            inputFluxes<- BoundInFlux(inputFluxes)
         }
        .Object@initialValues=initialValues
        .Object@inputFluxes=inputFluxes
        .Object@solverfunc=solverfunc
        #if (pass==FALSE) validObject(.Object) #call of the ispector if not explicitly disabled
        if (pass==FALSE) correctnessOfNlModel(.Object) #call of the ispector if not explicitly disabled
        return(.Object)
    }
)

#################################################
setMethod(
   f= "getInFluxes",
   signature(object="NlModel"),
   definition=function(object){
       return(object@inputFluxes)
     }
)
#################################################
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
      ### This function is a stub
      # It only starts the thing ...    
      C=getC(x)
      t=getTimes(x)
      cul=getCumulativeC(x)
     # print(C)
      errorPlotC(C,t)
      plot(t,cul)
   }
)
#################################################
setMethod(
   f= "print",
      signature(x="NlModel"),
      definition=function(x){
      ### This function is a stub
      # It only starts the thing ...    
     # print("Hi there I am the method print for model objects. Change me if you can")
     # print(getC(x)[,1])
   }
)
#################################################
setMethod(
   f= "getNumberOfPools",
      signature(object="NlModel"),
      definition=function(object){
      return(length(object@initialValues))
   }
)
##################################################
#setMethod(
#   f= "summary",
#      signature(object="NlModel"),
#      definition=function(object){
#      ### This function is a stub
#      # It only starts the thing ...    
#      #print("Hi there, I am the method summarize for model objects. 
#      #      I summarize everything:\n
#      #      1.) I have not been fully implemented yet.\n
#      #      2.) here comes the C stock at least.")
#      #print(getC(object)[,1])
#   }
#)
#################################################
setMethod(
   f= "show",
      signature(object="NlModel"),
      definition=function(object){
      ### This function is a stub
      # It only starts the thing ...    
      #print("Hi there I am the method show for model objects")
      #print(getC(object)[,1])
   }
)
#################################################
setMethod(
   f= "getDecompOp",
      signature= "NlModel",
      definition=function(object){
      ### This method returns the Decomposition Operator of the Model
      return(object@DepComp)
   }
)
#################################################
setMethod(
   f= "getParticleMonteCarloSimulator",
      signature= "NlModel",
      definition=function(object){
      ### This method creates a particle simulator 
      return(new(Class="MCSim",object))
   }
)
#################################################
setMethod(
   f= "getTimes",
      signature= "NlModel",
      definition=function(object){
      ### This functions extracts the times argument from an object of class NlModel
         times=matrix(ncol=1,object@times)
         colnames(times)="times"
      return(times)
   }
)
#################################################
setMethod(
   f= "getCumulativeC",
      signature= "NlModel",
      definition=function(object){
      ### This functions sums all C pools for every time stepobject of class NlModel
         Cpools=getC(object)
         cul=matrix(nrow=nrow(Cpools),0)
         for (i in 1:ncol(Cpools)){cul=cul+Cpools[,i]}
      return(cul)
   }
)
#################################################
setMethod(
   f= "getInitialValues",
      signature= "NlModel",
      definition=function(object){
      ### This functions extracts the initial values argument from an object of class NlModel
      return(object@initialValues)
   }
)
#################################################
res2fun=function(times,C){
  cn=colnames(C)
  if (length(cn)==0){cn=1:nrow(C)}
  Cs=list()#create an indexed list of functions
  for (i in 1:ncol(C)){
    key=cn[[i]]
    Cs[[key]] <- splinefun(times,C[,i])
  }
  return(Cs)
}
################################################
setMethod(
   f= "getTransferCoefficients",
      signature= "NlModel",
      definition=function # the transfer coefficients for all pool combinations as functions of time
      (object,as.closures=F){
      ### This functions returns the transfer coefficients for all pool combinations. 
      C=getC(object)
      #pp("C",environment())
      t=object@times
      #pp("t",environment())
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
################################################
setMethod(
   f= "getOutputFluxes",
      signature= "NlModel",
      definition=function # complete output of all pools, including the part transfered to other pools.
      (object,as.closures=F){
      ### This functions computes the output flux for all pools. Note that for any given pool not all the output of the pool is released from the system because it migtht as well be fed into other pools. If you are interested what a pool releases from the system use the method \code{\link{getReleaseFlux}}, which internally makes use of this method but afterwards substracts all parts of the outputs  that are fed to other pools.
      C=getC(object)
      t=getTimes(object)
      DepComp=object@DepComp
      DotO=getDotOut(DepComp) #this is a function of C and t 
      single_O=function(i){DotO(C[i,],t[[i]])}
      all_O=t(mapply(single_O,(1:length(t))))
      if (as.closures){
        return(res2fun(t,all_O))
      }else{
        return(all_O)
      }
   }
)
#------------------------------------------------------------------------------------
setMethod(
   f= "getReleaseFlux",
      signature= "NlModel",
      definition=function # get the release rate for all pools 
      ### The method computes the release of carbon per time for all points in time 
      ### specified in the objects time slot.
      (
      object ##<< an object of class NlModel
      ){
      DecOp=object@DepComp

      DotO=getDotOut(DecOp) #this is a function of C and t 
      Tr=getTransferMatrix(DecOp)#note that this is a function of C,t
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
#################################################
setMethod(
   f= "computeResults",
      signature= "NlModel",
      definition=function(object){

}
)
#################################################
setMethod(
   f= "getC",
      signature= c("NlModel"),
      definition=function # compute the solution for the content of the pools
      ### This function computes the value for C for each time and pool. 
      (
        object,           ##<< the model 
        as.closures=FALSE ##<< a flag that in TRUE will return an approximating function instead of the values
      ){
      times=object@times
      DepComp=object@DepComp
      DotO=getDotOut(DepComp) #this is a function of C and t 
      Tr=getTransferMatrix(DepComp) #this is also a function of C and t 
      force(DotO) 
      itm=object@inputFluxes
      inputrates=getFunctionDefinition(itm)
      #print(input)
      DotC=function(C,t){
        return(Tr(C,t)%*%DotO(C,t)+inputrates(t))
        #return(DotO(C,t)+inputrates(t))
      }
      sVmat=matrix(object@initialValues,ncol=1)
      C=solver(times,DotC,sVmat,object@solverfunc) 
      ### A matrix. Every column represents a pool and every row a point in time
      
      
      # check the result for negative values
      prec=1e-5
      pos=which(C< -prec,arr.ind=T)
      timesOfNegativeCStocks=times[pos[,1]]
      if(length(timesOfNegativeCStocks)>0){
        #print(timesOfNegativeCStocks)
        errorPlotC(C,times)
        stop("negative CStocks")
      }
      #f=function(i){paste("C",i,sep="")}
      #colnames(C)=sapply((1:ncol(C)),f)
      
      # check the actual decay rates for sanity
      # since in this nonlinear case the decay rates (the diagonal entries of N) 
      # can be dependent on C and therefor on the actual trajectory we
      # can generally only test after we have computed the solution.
      #print(C)
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
#################################################

setMethod(
  f="[",
  signature(x="NlModel",i="character"),#j="ANY",drop="ANY"), #since [] is a already defined generic the names of the arguments are not arbitrary 
  definition=function # overloads the [] operator
  ### This function is still experimental and might change considerably 
  ### It is meant as an alternative to all the get methods to provide 
  ### a more R like interface.
  (
    x, ##<< the model
    i  ##<< the property to access
  ){
      getSingleCol=function(slot_name){
          res=""
          #print(paste(sep="",">",slot_name,"<"))
          if(slot_name=="times"){ res=getTimes(x)}
          if(slot_name=="C"){ res=getC(x)}
          #if(slot_name=="ReleaseFlux"){ res=getReleaseFlux(x)}
          #if(slot_name=="AccumulatedRelease"){ res=getAccumulatedRelease(x)}
          #if(res==""){stop(paste("The slot",slot_name,"is not defined"))}
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
#################################################
setMethod("$",signature(x="NlModel"), #since $ is a already defined generic the names of the arguments are not arbitrary 
        definition=function # overload the $ operator
        ### This function is still experimental and might change considerably 
        ### It is meant as an alternative to all the get methods to provide 
        ### a more R like interface.
        ( 
          x, ##<< the model
          name ##<< the property to access
        ){
            return(x[name])
        }
)

