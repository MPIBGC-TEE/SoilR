
#' common class for pool ids 
#'
#' examples for ids are index or name
#' @s4methods
setClass(
   Class="PoolId",
   contains=c("VIRTUAL")
)


#' S4 class for pool indices 
#'
#' used to dispatch pool index specific methods like conversion to names.
#' @s4methods
setClass(
   Class="PoolIndex",
   ,contains=c('PoolId','integer')
)


#' class for pool-name-strings
#'
#' used to control the creation of PoolName objects which have to be valid R identifiers 
#' and to dispatch pool name specific methods like conversion to pool indices 
#' @s4methods
setClass(
   Class="PoolName",
   ,contains=c('PoolId','character')
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

#' @s4methods
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

#' S4 class for a scalar time dependent function on a finite time interval
#'
#' @s4methods
setClass(
   Class="ScalarTimeMap",
   contains=c('TimeMap')
)

#'Constructor for the class with the same name
#'
#'@slot destinationIndex 
#'@slot flux 
StateIndependentInFlux_by_PoolIndex<-setClass(
  Class="StateIndependentInFlux_by_PoolIndex",
  slots=c(destinationIndex='PoolIndex',flux='ScalarTimeMap')
)

#' Subclass of list that is guaranteed to contain only elements of type
#' \linkS4class{StateIndependentInFlux_by_PoolIndex}
#'
#' @s4methods
setClass(
  Class = "StateIndependentInFluxList_by_PoolIndex",
  contains=c("list")
)


#' Subclass of list that is guaranteed to contain only elements of type
#' \linkS4class{ConstantInFlux_by_PoolIndex}
#'
#' @s4methods
setClass(
  Class = "ConstantInFluxList_by_PoolIndex",
  contains=c("list")
)

#' S4-class for a single internal flux wiht source and destination pools specified by indices 
#'
#' @s4methods
setClass(
  Class="InternalFlux_by_PoolIndex",
  # contains="PoolConnection", we do not want to iherit mehtods...
  slots=c(sourceIndex='PoolIndex',destinationIndex='PoolIndex',func='function')
)

#' S4-class for a single internal flux wiht source and destination pools specified by name
#'
#' @s4methods
setClass(
  Class="InternalFlux_by_PoolName",
  # contains="PoolConnection", we do not want to iherit mehtods...
  slots=c(sourceName='PoolName',destinationName='PoolName',func='function')
)


#' S4-class for a list of internal fluxes with source and destination pool inidices 
#'
#' @s4methods
setClass(
  Class = "InternalFluxList_by_PoolIndex",
  contains=c("list")
)


#' S4-class for a list of internal fluxes with indexed by (source and destination pool) names
#'
#' @s4methods
setClass(
  Class = "InternalFluxList_by_PoolName",
  contains=c("list")
)

#' S4 class for a single out-flux with source pool index
#'
#' @s4methods
setClass(
  Class="OutFlux_by_PoolIndex",
  # contains="PoolConnection", we do not want to iherit mehtods...
  slots=c(sourceIndex='PoolIndex',func='function')
)

#' S4 class for a single out-flux with source pool name 
#'
#' @s4methods
setClass(
  Class = "OutFlux_by_PoolName",
  # contains="PoolConnection", we do not want to iherit mehtods...
  slots=c(sourceName='PoolName',func='function')
)


#' S4 class for a list of out-fluxes indexed by source pool name 
#'
#' @s4methods
setClass(
  Class = "OutFluxList_by_PoolName",
  contains=c("list")
)

#' A virtual S4-class representing (different subclasses) of in-fluxes to the system
#' 
#' @s4methods
setClass(
   Class="InFluxes",
   contains="VIRTUAL"
)
#--------------------------------
#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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

#' @s4methods
setClass(
   Class="ConstInFluxes",
   contains=c("InFluxes"),
   slots=list(
    map="numeric"
   )
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
#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
#' @s4methods
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
#' S4-class to represent atmospheric 14C concentration as
#' scalar function of time.
#' 
#' As time dependent scalar function which remembers its
#' domain ( see \code{\linkS4class{ScalarTimeMap}}) and its
#' format. 
#' @autocomment 
setClass(
    Class="BoundFc",
    contains=c("ScalarTimeMap","Fc")
)

#' S4-class to represent compartmental operators 
#'
#' @s4methods
setClass(
    Class="DecompOp",
    ,
    contains="VIRTUAL"
)

#--------------------------------
#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
#' @s4methods
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
#' @s4methods
setClass(
    Class="ConstLinDecompOp",
    contains=c("DecompOp"),
    slots=list( mat="matrix")
)

#' A class to represent a constant (=nonautonomuous,linear) compartmental matrix 
#' with a time dependent (linear) scalar pre factor 
#' This is a special case of a linear compartmental operator/matrix 
#' @s4methods
setClass(
    Class="ConstLinDecompOpWithLinearScalarFactor"
    ,contains=c("DecompOp")
    ,slots=list(clo="ConstLinDecompOp",xi='TimeMap')
)

#' A S4 class to represent a linear compartmental operator 
#' defined on time interval
#'
#' @s4methods
setClass(
    Class="BoundLinDecompOp",
    contains=c("DecompOp","TimeMap"),   
   )

#' An S4 class to represent the of  nonlinear nonautonomuous compartmental system independently of the order of state variables 
#'
#' @s4methods
setClass(
    Class="UnBoundNonLinDecompOp_by_PoolNames",
    slots=c(
        internal_fluxes="InternalFluxList_by_PoolName"
        ,out_fluxes="OutFluxList_by_PoolName" 
        ,timeSymbol="character"
    )
)

#--------------------------------
#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
#' @s4methods
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
#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
#' @s4methods
setClass(
   Class="BoundInFluxes",
   contains=c("InFluxes","TimeMap"),
)



#' class for a constan influx to a single pool identified by index
#'
#' @s4methods
setClass(
  Class="ConstantInFlux_by_PoolIndex",
  slots=c(destinationIndex='PoolIndex',flux_constant='numeric')
)

#' S4 class for the influx to a single pool identified by theindex 
#'
#' @s4methods
setClass(
  Class="InFlux_by_PoolIndex",
  # contains="PoolConnection", we do not want to iherit mehtods...
  slots=c(destinationIndex='PoolIndex',func='function')
)

#' S4 class for the influx to a single pool identified by the name 
#'
#' @s4methods
setClass(
  Class = "InFlux_by_PoolName",
  # contains="PoolConnection", we do not want to iherit mehtods...
  slots=c(destinationName='PoolName',func='function')
)


#' Class for a list of influxes indexed by the names of the target pools 
#'
#' @s4methods
setClass(
  Class = "InFluxList_by_PoolName",
  contains=c("list")
)

#' S4 class representing a constant internal flux rate
#'
#' The class is used to dispatch specific methods for the creation of the compartmental matrix which is simplified in case of constant rates.
#' @s4methods
setClass(
  Class="ConstantInternalFluxRate_by_PoolIndex",
  slots=c(sourceIndex='PoolIndex',destinationIndex='PoolIndex',rate_constant='numeric')
)

#' S4-class to represent a constant internal flux rate with source and target indexed by name 
#'
#' @s4methods
setClass(
  Class="ConstantInternalFluxRate_by_PoolName",
  slots=c(sourceName="PoolName",destinationName='PoolName',rate_constant='numeric')
)

#' S4 Class to represent a single constant out-flux rate with the 
#' source pool specified by an index
#'
#' @s4methods
setClass(
  Class="ConstantOutFluxRate_by_PoolIndex",
  slots=c(sourceIndex='PoolId',rate_constant='numeric')
)

#' S4 Class to represent a single constant out-flux rate with the 
#' source pool specified by name 
#'
#' @s4methods
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
#' @s4methods
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
#' automatic title
#' 
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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



#' automatic title
#' 
#' @param .Object no manual documentation
#' @param times no manual documentation
#' @param mat no manual documentation
#' @param initialValues no manual documentation
#' @param inputFluxes no manual documentation
#' @param solverfunc no manual documentation
#' @param pass no manual documentation
#' @param timeSymbol no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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

#' S4 class representing a model run 
#'
#' @s4methods
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

#' S4 class representing a constan ^{14}C fraction
#'
#' @s4methods
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
#' @s4methods
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
