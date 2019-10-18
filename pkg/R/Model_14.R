
#' @auto

#' @auto

#' @auto
setMethod(
    f="initialize",
    signature=c("Model_14"),
    definition=function 
    (
        .Object,              
        times=c(0,1),         
        mat=ConstLinDecompOp(matrix(nrow=1,ncol=1,0)),
        initialValues=numeric()
        ,
        initialValF=ConstFc(values=c(0),format="Delta14C")      
        ,
        inputFluxes= BoundInFluxes(
            function(t){
                return(matrix(nrow=1,ncol=1,1))
            },
            0,
            1
        )
        ,
        c14Fraction=BoundFc(
            function(t){
                return(matrix(nrow=1,ncol=1,1))
            },
            0,
            1
        )   
        ,
        c14DecayRate=0
        ,
        solverfunc=deSolve.lsoda.wrapper
        ,
        pass=FALSE
     ){
        .Object <- callNextMethod(.Object,times,mat,initialValues,inputFluxes,solverfunc,pass=pass)
        .Object@initialValF=initialValF
        .Object@c14Fraction=c14Fraction
        .Object@c14DecayRate=c14DecayRate
        if (pass==FALSE) validObject(.Object) 
        return(.Object)
    }
)






#' general constructor for class Model_14
#' 
#' This method tries to create an object from any combination of arguments that
#' can be converted into the required set of building blocks for the Model_14
#' for n arbitrarily connected pools.
#' 
#' 
#' @param t A vector containing the points in time where the solution is
#' sought.
#' @param A something that can be converted by \link{GeneralDecompOp} to any of
#' the available subclasses of \code{\linkS4class{DecompOp}}.
#' @param ivList A vector containing the initial amount of carbon for the n
#' pools. The length of this vector is equal to the number of pools and thus
#' equal to the length of k. This is checked by an internal function.
#' @param initialValF An object equal or equivalent to class ConstFc containing
#' a vector with the initial values of the radiocarbon fraction for each pool
#' and a format string describing in which format the values are given.
#' @param inputFluxes something that can be converted by \link{InFluxes}
#' to any of the available subclasses of \linkS4class{InFluxes}.
#' @param inputFc An object describing the fraction of C_14 in per mille
#' (different formats are possible)
#' @param c14DecayRate the rate at which C_14 decays radioactivly. If you don't
#' provide a value here we assume the following value: k=-0.0001209681 y^-1 .
#' This has the side effect that all your time related data are treated as if
#' the time unit was year. Thus beside time itself it also affects decay rates
#' the inputrates and the output
#' @param solverfunc The function used by to actually solve the ODE system.
#' This can be \code{\link{deSolve.lsoda.wrapper}} or any other user provided
#' function with the same interface.
#' @param pass Forces the constructor to create the model even if it is invalid
#' @return A model object that can be further queried.
#' @seealso \code{\link{TwopParallelModel}}, \code{\link{TwopSeriesModel}},
#' \code{\link{TwopFeedbackModel}}
#' @examples
#' # examples from external files
#' # inst/tests/requireSoilR/runit.all.possible.Model.arguments.R test.all.possible.Model.arguments:
#' 
#'   # This example shows different kinds of arguments to the function Model.
#'   # The model objects we will build will share some common features.
#'   #  - two pools 
#'   #  - initial values 
#' 
#'        iv<-  c(5,6)
#' 
#'   #  - times 
#' 
#'        times <- seq(1,10,by=0.1)
#' 
#'   # The other parameters A and inputFluxes will be different
#'   # The function Model will transform these arguments 
#'   # into objects of the classes required by the internal constructor.
#'   # This leads to a number of possible argument types. 
#'   # We demonstrate some of the possibilities here.
#'   # Let us first look at the choeices for argument 'A'.
#'   
#'   #) 
#'   possibleAs  <- example.2DGeneralDecompOpArgs()
#'   
#'   # Since "Model" will call "InFluxes" on its "inputFluxes" 
#'   # argument there are again different choices
#'   # we have included a function in SoilR that produces 2D examples
#'   
#'   possibleInfluxes <- example.2DInFlux.Args()
#'  print(possibleInfluxes$I.vec)
#'   # We can build a lot of  models from the possible combinations
#'   # for instance   
#'   #m1 <- Model(
#'   #        t=times,
#'   #        A=matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2)),
#'   #        ivList=iv,
#'   #        inputFluxes=possibleInfluxes$I.vec) 
#'   ## We now produce that all combinations of As and InputFluxes
#'   combinations <- listProduct(possibleAs,possibleInfluxes)
#'   print(length(combinations))
#'   # an a Model for each
#'   models <- lapply(
#'               combinations,
#'               function(combi){
#'                 #Model(t=times,A=combi$A,ivList=iv,inputFluxes=combi$I)
#'                 Model(t=times,A=combi[[1]],ivList=iv,inputFluxes=combi[[2]])
#'               }
#'             )
#'   ## lets check that we can compute something# 
#'   lapply(models,getC)
#' 
#' # inst/examples/ModelExamples.R CorrectNonautonomousLinearModelExplicit:
#' 
#'   # This example describes the creation and use of a Model object that 
#'   # is defined by time dependent functions for decomposition and influx.
#'   # The constructor of the Model-class  (see  ?Model) 
#'   # works for different combinations of 
#'   # arguments.
#'   # Although Model (the constructor function for objects of this class 
#'   # accepts many many more convienient kinds of arguments,
#'   # we will in this example call the constructor whith arguments which 
#'   # are of the same type as one of hte current internal 
#'   # representations in the 
#'   # Model object and create these arguments explicitly beforehand 
#'   # to demonstrate the approach with the most flexibility.
#'   # We start with the Decomposition Operator.
#'   # For this example we assume that we are able to describe the
#'   # decomposition ofperator  by explicit R functions that are valid 
#'   # for a finite time interval.
#'   # Therefore we choose the appropriate  sub class BoundLinDecompOp
#'   # of DecompOp explicitly.  (see ?'BoundLinDecompOp-class') 
#'   A=BoundLinDecompOp(
#'     ## We call the generic constructor (see ?BoundLindDcompOp) 
#'     ## which has a method  
#'     ## that takes a matrix-valued function of time as its first argument.
#'     ## (Although Model accepts time series data directly and 
#'     ## will derive the internally used interpolating for you, 
#'     ## the function argument could for instance represent the result
#'     ## of a very sophisticated interpolation performed by yourself)
#'     function(t){
#'       matrix(nrow=3,ncol=3,byrow=TRUE,
#'          c(
#'            -1,    0,        0,
#'           0.5,   -2,        0,
#'             0,    1, sin(t)-1 
#'         )
#'       )    
#'     },
#'     ## The other two arguments describe the time interval where the 
#'     ## function is valid (the domain of the function)
#'     ## The interval will be checked against the domain of the InFlux
#'     ## argument of Model and against its 't' argument to avoid 
#'     ## invalid computations outside the domain. 
#'     ## (Inf and -Inf are possible values, but should only be used 
#'     ## if the function is really valid for all times, which is 
#'     ## especially untrue for functions resulting from interpolations,
#'     ## which are usually extremely misleading for arguments outside the 
#'     ## domain covered by the data that has been used for the interpolation.)
#'     ## This is a safety net against wrong results origination from unitendet EXTRApolation )
#'     starttime=0,
#'     endtime=20
#'   )  
#'   I=BoundInFluxes(
#'      ## The first argument is a vector-valued function of time
#'      function(t){
#'        matrix(nrow=3,ncol=1,byrow=TRUE,
#'            c(-1,    0,    0)
#'        )
#'      },
#'      ## The other two arguments describe the time interval where the 
#'      ## function is valid (the domain of the function)
#'      starttime=0,
#'      endtime=40
#'   )
#'   ## No we specify the points in time where we want 
#'   ## to compute results
#'   t_start=0 
#'   t_end=10 
#'   tn=50
#'   timestep <- (t_end-t_start)/tn 
#'   times <- seq(t_start,t_end,timestep) 
#'   ## and the start values
#'   sv=c(0,0,0)
#'   mod=Model(t=times,A,sv,I)
#' 
#'   ## No we use the model to compute some results
#'   getC(mod)
#'   getReleaseFlux(mod)
#'   #also look at the methods section of Model-class 
#' 
Model_14 <- function 
  (t,			
   A,			
   ivList,		
   initialValF, 
   inputFluxes, 
   inputFc,
   c14DecayRate =-0.0001209681 ,  
   solverfunc=deSolve.lsoda.wrapper,		
   pass=FALSE  
   )
  {
     obj=new(Class="Model_14",t,GeneralDecompOp(A),ivList, initialValF,InFluxes(inputFluxes),inputFc,c14DecayRate=c14DecayRate,solverfunc=solverfunc,pass=pass)
     return(obj)
  }

#' @auto

#' @auto

#' @auto
setMethod(
   f= "getC14",
      signature= "Model_14",
      definition=function(object){
      ns=length(object@initialValues)
      Atm=object@mat
      A=getFunctionDefinition(Atm)
      k=object@c14DecayRate
      m=diag(rep(k,ns),nrow=ns) 
      A_C14=function(t){
          Aorg=A(t)
          newA=Aorg+m
          return(newA)
      }
      itm   <- object@inputFluxes
      input <- getFunctionDefinition(itm)
      Fctm  <- object@c14Fraction
      F0    <- object@initialValF
      Fctm  <- AbsoluteFractionModern(Fctm)
      F0    <- AbsoluteFractionModern(F0)
      Fc=getFunctionDefinition(Fctm)
      input_C14=function(t){
          return(Fc(t)*input(t))
      }
      ydot=NpYdot(A_C14,input_C14)
      inivals=getValues(F0)
      sVmat=matrix(inivals*object@initialValues,nrow=ns,ncol=1) 
      Y=solver(object@times,ydot,sVmat,object@solverfunc) 
      return(Y)
   }
)

#' @auto

#' @auto

#' @auto
setMethod(
   f= "getF14",
      signature= "Model_14",
      definition=function 
      (
      object 
      ){
      C=getC(object) 
      C14=getC14(object) 
      fr=C14/C
      fr=Delta14C_from_AbsoluteFractionModern(fr)
      return(fr)
   }
)

#' @auto

#' @auto

#' @auto
setMethod(
   f= "getReleaseFlux14",
      signature= "Model_14",
      definition=function 
      (
        object 
      )
      {
      C14=getC14(object) 
      times=object@times
      Atm=object@mat
      A=getFunctionDefinition(Atm)
      n=length(object@initialValues)
      rfunc=RespirationCoefficients(A)
      if (n==1) { r=matrix(ncol=n,sapply(times,rfunc))}
      else {r=t(sapply(times,rfunc))}
      R=r*C14
      return(R)
   }
)

#' @auto

#' @auto

#' @auto
setMethod(
  f= "getF14R",
  signature= "Model_14",
  definition=function 
    (
     object 
     ){
    R=getReleaseFlux(object) 
    R14=getReleaseFlux14(object) 
    fr=rowSums(R14)/rowSums(R)
    fr=Delta14C_from_AbsoluteFractionModern(fr)
    return(fr)
  }
  )

#' @auto

#' @auto

#' @auto
setMethod(
  f= "getF14C",
  signature= "Model_14",
  definition=function
  (
  object
  ){
    C=getC(object) 
    C14=getC14(object) 
    fr=rowSums(C14)/rowSums(C)
    fr=Delta14C_from_AbsoluteFractionModern(fr)
    return(fr)
  }
  )
