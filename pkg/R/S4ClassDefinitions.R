#' Subclass of list that is guaranteed to contain only elements of type
#' \linkS4class{ConstantInFlux_by_PoolIndex}
#'
setClass(
  Class = "ConstantInFluxList_by_PoolIndex",
  contains=c("list")
)

#' S4-class for a single internal flux wiht source and destination pools specified by name
#'
setClass(
  Class="InternalFlux_by_PoolIndex",
  # contains="PoolConnection", we do not want to iherit mehtods...
  slots=c(sourceIndex='PoolIndex',destinationIndex='PoolIndex',func='function')
)

#' S4-class for a list of internal fluxes with source and destination pool inidices 
#'
setClass(
  Class = "InternalFluxList_by_PoolIndex",
  contains=c("list")
)


#' S4-class for a list of internal fluxes with indexed by (source and destination pool) names
#'
setClass(
  Class = "InternalFluxList_by_PoolName",
  contains=c("list")
)

#' S4-class for a single out fluxe with source pool index
#'
setClass(
  Class="OutFlux_by_PoolIndex",
  # contains="PoolConnection", we do not want to iherit mehtods...
  slots=c(sourceIndex='PoolIndex',func='function')
)

setClass(
  Class = "OutFluxList_by_PoolName",
  contains=c("list")
)

#' A virtual S4-class representing (different subclasses) of in-fluxes to the system
#' 
setClass(
   Class="InFluxes",
   contains="VIRTUAL"
)
#--------------------------------
StateDependentInFluxVector<-setClass(
   Class="StateDependentInFluxVector"
   ,contains="InFluxes"
   ,slots=list(
      map="function"
      #,
      #lag="numeric"
      ,
      starttime="numeric"
      ,
      endtime="numeric"
   )
)
#--------------------------------

#' S4 class for a constant influx vector 
#' 
#' It is mainly used to dispatch S4-methods for computations that 
#' are valid only if the influx is constant.
#' This knowledge can either be used to speed up computations or to decide if they are
#' possible at all.
#' E.g. the computation of equilibria for a model run requires autonomy of the model which 
#' requires the influxes to be time independent. If the model is linear  
#' and compartmental then the (unique) equilibrium can be computed.
#' Accordingly a method with ConstInFluxes in the signature can be implemented, whereas 
#' none would be available for a general InFluxes argument.

setClass(
   Class="ConstInFluxes",
   contains=c("InFluxes"),
   slots=list(
    map="numeric"
   )
)

#' S4 class for a time dependent function 
#' 
#' The class represents functions which are defined on a (possibly infinite) 
#' interval from [starttime,endtime]
#' Instances are usually created internally from data frames or lists provided by the user in the high level interfaces.
#' 
#' The class is necessary to be able to detect unwanted extrapolation of 
#' time line data which might otherwise occur for some of the following 
#' reasons:
#' SoilR allows to specify measured data for many of its arguments
#' and computes the interpolating functions automatically.
#' The functions returned by the standard R interpolation mechanisms
#' like \code{splinefun} or \code{approxfun} do not provide a safeguard 
#' against accidental extrapolation.  
#' Internally SoilR converts nearly all data to time dependent functions 
#' e.g. to be used in ode solvers. So the information of the domain of the
#' function has to be kept.

setClass(
   Class="TimeMap",
   slots=list(
      map="function"
      #,
      #lag="numeric"
      ,
      starttime="numeric"
      ,
      endtime="numeric"
   )
)
#--------------------------------

#' S4 class for a scalar time dependent function on a finite time interval
setClass(
   Class="ScalarTimeMap",
   contains=c('TimeMap')
)

#--------------------------------
correctnessOfFc=function
(object 
)
{
   res=TRUE
   supported_formats <- c("Delta14C","AbsoluteFractionModern")
   f=object@format
   if (!any(grepl(f,supported_formats))){
      err_str=cat("The required format:",f," describing the c_14 fraction is not supported.\n 
   	     The supported formats are: ",supported_formats,". \n",sep="")
      stop(simpleError(err_str))
      return(res)
   }
}
setClass( 
    Class="Fc",
    ,
    contains="VIRTUAL"
    ,
    slots=c(format='character')
    ,
    validity=correctnessOfFc 
)
#--------------------------------
setClass(
    Class="BoundFc",
    contains=c("TimeMap","Fc")
)

#' S4-class to represent compartmental operators 
#'
setClass(
    Class="DecompOp",
    ,
    contains="VIRTUAL"
)

#--------------------------------
setClass(
    Class="DecompositionOperator",
    contains="DecompOp",   
    slots=list(
    map="function"
    ,
    lag="numeric"
    ,
    starttime="numeric"
    ,
    endtime="numeric"
    ) 
)

#--------------------------------
#' A class to represent a constant (=nonautonomuous,linear) compartmental matrix 
#' or equivalently a combination of ordered constant internal flux rates and 
#' constant out flux rates.
setClass(
    Class="ConstLinDecompOp",
    contains=c("DecompOp"),
    slots=list( mat="matrix")
)


#--------------------------------
#' An S4 class to represent the of  nonlinear nonautonomuous compartmental system independently of the order of state variables 
#'
setClass(
    Class="UnBoundNonLinDecompOp_by_PoolNames",
    slots=c(
        internal_fluxes="InternalFluxList_by_PoolName"
        ,out_fluxes="OutFluxList_by_PoolName" 
        ,timeSymbol="character"
    )
)

#--------------------------------
setClass(
   Class="TransportDecompositionOperator",
   contains="TimeMap",
   slots=list(
    numberOfPools="numeric"
    ,
    alpha="list"
    ,
    f="function"
   )
) 

#--------------------------------
setClass(
   Class="BoundInFluxes",
   contains=c("InFluxes","TimeMap"),
)

#--------------------------------

#' common class for pool is 
#'
#' examples for ids are index or name
setClass(
   Class="PoolId",
   contains=c("VIRTUAL")
)


#--------------------------------

#' class for pool indices 
#'
#' used to dispatch pool index specific methods like conversion to names.
setClass(
   Class="PoolIndex",
   ,contains=c('PoolId','integer')
)

#--------------------------------

#' class for pool-name-strings
#'
#' used to control the creation of PoolName objects which have ot be valid R identifiers and to dispatch pool name specific methods like conversion to pool indexed 
setClass(
   Class="PoolName",
   ,contains=c('PoolId','character')
)


#' class for a constan influx to a single pool identified by index
#'
setClass(
  Class="ConstantInFlux_by_PoolIndex",
  slots=c(destinationIndex='PoolIndex',flux_constant='numeric')
)

#--------------------------------

#' class for a constan influx to a single pool identified by the name 
#'
setClass(
  Class = "InFlux_by_PoolName",
  # contains="PoolConnection", we do not want to iherit mehtods...
  slots=c(destinationName='PoolName',func='function')
)


#' Class for a list of influxes indexed by the names of the target pools 
#'
setClass(
  Class = "InFluxList_by_PoolName",
  contains=c("list")
)

#' S4 class representing a constant internal flux rate
#'
#' The class is used to dispatch specific methods for the creation of the compartmental matrix which is simplified in case of constant rates.
setClass(
  Class="ConstantInternalFluxRate_by_PoolIndex",
  slots=c(sourceIndex='PoolIndex',destinationIndex='PoolIndex',rate_constant='numeric')
)

#' S4-class to represent a constant internal flux rate with source and target indexed by name 
#'
setClass(
  Class="ConstantInternalFluxRate_by_PoolName",
  slots=c(sourceName="PoolName",destinationName='PoolName',rate_constant='numeric')
)

#' S4 Class to represent a single constant out-flux rate with the 
#' source pool specified by an index
#'
setClass(
  Class="ConstantOutFluxRate_by_PoolIndex",
  slots=c(sourceIndex='PoolId',rate_constant='numeric')
)

#' S4 Class to represent a single constant out-flux rate with the 
#' source pool specified by name 
#'
setClass(
  Class="ConstantOutFluxRate_by_PoolName",
  slots=c(sourceName='PoolName',rate_constant='numeric')
)

#--------------------------------
is.negative=function(number){
   return(number<0)
}
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

#' deprecated class for a non-linear model run. 
#'
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
#-----------------------------------------
correctnessOfModel_by_PoolNames<-function(object){
    TRUE
}
Model_by_PoolNames=setClass(
    Class="Model_by_PoolNames"
    ,slots=c(
         times="numeric"
        ,mat="UnBoundNonLinDecompOp_by_PoolNames"
        ,initialValues="numeric"
        ,inputFluxes="InFluxList_by_PoolName"
        ,solverfunc="function"
        ,timeSymbol='character'
   )
   #,validity=correctnessOfModel_by_PoolNames
)
setMethod(
    'initialize'
    ,signature= signature(.Object='Model_by_PoolNames')
    ,definition=function(
        .Object
        ,times
        ,mat
        ,initialValues
        ,inputFluxes
        ,solverfunc
        ,pass
        ,timeSymbol
    ){
        .Object@times=times
        .Object@mat=mat
        .Object@initialValues=initialValues
        .Object@inputFluxes=inputFluxes
        .Object@solverfunc=solverfunc
        .Object@timeSymbol=timeSymbol
        .Object
    }
)
#--------------------------------
correctnessOfModel <- function(object){   
    times=object@times
    Atm=object@mat
    ivList=object@initialValues
    InFluxes=object@inputFluxes
    A=getFunctionDefinition(Atm)
    na=nrow(A(0))
    rcoeffs=RespirationCoefficients(A)
    r=sapply(times,rcoeffs)
    truthv=sapply(r,is.negative)
    positions=grep("TRUE",truthv)
    res=TRUE
    if (length(positions)>0){
       stop(simpleError("The following columns contain unreasonable entries that lead to negative respirations for these pools. Please check your matrix as function of time."))
        }
    tA_min=getTimeRange(Atm)["t_min"]
    tA_max=getTimeRange(Atm)["t_max"]
    tI_min=getTimeRange(InFluxes)["t_min"]
    tI_max=getTimeRange(InFluxes)["t_max"]
    t_min=min(times)
    t_max=max(times)
    haveALook="Have look at the object containing  A(t) or the data it is created from\n"
    if (t_min<tA_min) {
        stop(
          simpleError(
            paste(
              "You ordered a timeinterval that starts earlier than the interval your matrix valued function A(t) is defined for. \n "
              ,haveALook
              ,paste('tA_min=',tA_min,'\n')
              ,paste('t_min=',t_min,'\n')
            )
          )
        )
    }
    if (t_max>tA_max) {
        stop(
          simpleError(
            paste(
              "You ordered a timeinterval that ends later than the interval your matrix valued function A(t) is defined for. \n "
              ,haveALook
              ,paste('tA_max=',tA_max,'\n')
              ,paste('t_max=',t_max,'\n')
            )
          )
        )
    }
    if (t_min<tI_min) {
        stop(
          simpleError(
            paste(
              "You ordered a timeinterval that starts earlier than the interval your function I(t) (InFluxes) is defined for. \n ",
              haveALook
            )
          )
        )
    }
    if (t_max>tI_max) {
        stop(
          simpleError(
            paste(
              "You ordered a timeinterval that ends later than the interval your function I(t) (InFluxes) is defined for. \n ",
              haveALook
            )
          )
        )
    }
    return(res)
}
setClass(
   Class="Model",
   representation=representation(
        times="numeric"
        ,
        mat="DecompOp"
        ,
        initialValues="numeric"
        ,
        inputFluxes="InFluxes"
        ,
        solverfunc="function"
   ) , 
   validity=correctnessOfModel 
)

#--------------------------------
setClass(
   Class="ConstFc",
   contains="Fc",
   slots=c(values="numeric")
)

#--------------------------------
correctnessOfModel14=function
(object 
)
{
t_min=min(object@times)
t_max=max(object@times)
atm_c14 <- object@c14Fraction
tA_min=getTimeRange(atm_c14)["t_min"]
tA_max=getTimeRange(atm_c14)["t_max"]
    if (t_min<tA_min) {
        stop(simpleError(sprintf("You ordered a timeinterval that starts earlier (t_min=%s) than the interval your atmospheric 14C fraction is defined for (tA_min=%s). \n Have look at the object or the data it is created from",t_min,tA_min)))
    }
    if (t_max>tA_max) {
        stop(simpleError(sprintf("You ordered a timeinterval that ends later (tmax=%s) than the interval your  your atmospheric 14C fraction is defined for (tA_max=%s). \n Have look at the object or the data it is created from",t_max,tA_max)))
    }
}
#' S4-class to represent a ^{14}C model run 
#'
setClass(
    Class="Model_14",
    contains="Model",
    representation=representation(
        c14Fraction="BoundFc",
        c14DecayRate="numeric",
        initialValF="ConstFc"
    ) , 
    validity=correctnessOfModel14 
)

#--------------------------------
