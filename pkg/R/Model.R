correctnessOfModel <- function 
(object)
{   
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
is.negative=function(number){
   return(number<0)
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
setMethod(
    f="initialize",
    signature="Model",
    definition=function 
    (
        .Object,
        times=c(0,1),
        mat=ConstLinDecompOp(matrix(nrow=1,ncol=1,0)), 
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
        .Object@mat=mat
        .Object@initialValues=initialValues
        .Object@inputFluxes=inputFluxes
        .Object@solverfunc=solverfunc
        if (pass==FALSE) correctnessOfModel(.Object) 
        return(.Object)
    }
)






#' Constructor for class \linkS4class{Model}
#' 
#' This function creates an object of class \linkS4class{Model}, The arguments
#' can be given in different form as long as they can be converted to the
#' necessary internal building blocks.  (See the links)
#' 
#' This function \code{Model} wraps the internal constructor of class
#' \linkS4class{Model}. The internal constructor requires the argument \code{A}
#' to be of class \linkS4class{DecompOp} and argument \code{inputFluxes} to be
#' of class \linkS4class{InFluxes}. Before calling the internal constructor
#' \code{Model} calls \link{GeneralDecompOp} on its argument \code{A} and
#' \link{GeneralInFlux} on its argument \code{inputFluxes} to convert them into
#' the required classes. Both are generic functions. Follow the links to see
#' for which kind of inputs conversion methods are available. The attempted
#' conversion allows great flexibility with respect to arguments and
#' independence from the actual implementation. However if your code uses the
#' wrong argument the error will most likely occur in the delegate functions.
#' If this happens analyse the errormassage (or use \code{traceback()}) to see
#' which function was called and try to call the constructor of the desired
#' subclass explicitly with your arguments.  The subclasses are linked in the
#' class documentation \linkS4class{DecompOp} or \linkS4class{InFluxes}
#' respectively.
#' 
#' Note also that this function checks its arguments quite elaborately and
#' tries to detect accidental unreasonable combinations, especially concerning
#' two kinds of errors. 
#' \enumerate{ 
#' \item unintended extrapolation of time series data 
#' \item violoations of massbalance by the DecompOp argument. 
#'}
#' 
#' SoilR has a lot of unit tests which are installed with the package and are
#' sometimes instructive as examples.  To see example scenarios for parameter
#' check look at:
#' \Sexpr{system.file('tests','runit.correctness_of_Model.R',package='SoilR')}
#' 
#' @param t A vector containing the points in time where the solution is
#' sought.
#' @param A something that can be converted by \link{GeneralDecompOp} to any of
#' the available subclasses of \code{\linkS4class{DecompOp}}.
#' @param ivList A numeric vector containing the initial amount of carbon for
#' the n pools. The length of this vector is equal to the number of pools. This
#' is checked by an internal function.
#' @param inputFluxes something that can be converted by \link{GeneralInFlux}
#' to any of the available subclasses of \linkS4class{InFluxes}.
#' @param solverfunc The function used to actually solve the ODE system. The
#' default is \code{\link{deSolve.lsoda.wrapper}} but you can also provide your
#' own function that the same interface.
#' @param pass Forces the constructor to create the model even if it does not
#' pass internal sanity checks
#' @return An object of class \linkS4class{Model} that can be queried by many
#' methods to be found there.
#' @seealso This function is called by many of the \link{predefinedModels}. \cr
#' Package functions called in the examples:\cr
#' \code{\link{example.2DInFlux.Args}},\cr
#' \code{\link{example.2DGeneralDecompOpArgs}},\cr
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
#'   # Since "Model" will call "GeneralInFlux" on its "inputFluxes" 
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
Model <- function 
  (t,			
   A,			
   ivList,		
   inputFluxes, 
   solverfunc=deSolve.lsoda.wrapper,		
   pass=FALSE  
   )
  {
     obj=new(Class="Model",t,GeneralDecompOp(A),ivList,InFluxes(inputFluxes),solverfunc,pass)
     return(obj)
  }
setMethod(
   f= "plot",
      signature(x="Model"),
      definition=function 
      (x){
      plot(getTimes(x),getC(x)[,1])
   }
)
setMethod(
   f= "print",
      signature(x="Model"),
      definition=function
      (x){
   }
)
setMethod(
   f= "summary",
      signature(object="Model"),
      definition=function 
      (object){
   }
)
setMethod(
   f= "show",
      signature(object="Model"),
      definition=function
      (object){
   }
)
setMethod(
   f= "getDecompOp",
      signature= "Model",
      definition=function(object){
      return(object@mat)
   }
)
setMethod(
   f= "getTimes",
      signature= "Model",
      definition=function(object){
         times=matrix(ncol=1,object@times)
         colnames(times)="times"
      return(times)
   }
)
setMethod(
   f= "getC",
      signature= "Model",
      definition=
      function(object){
      ns=length(object@initialValues)
      Atm=object@mat
      A=getFunctionDefinition(Atm)
      itm=object@inputFluxes
      input=getFunctionDefinition(itm)
      ydot=NpYdot(A,input)
      sVmat=matrix(object@initialValues,nrow=ns,ncol=1)
      Y=solver(object@times,ydot,sVmat,object@solverfunc) 
      f=function(i){paste("C",i,sep="")}
      return(Y)
   }
)
setMethod(
   f= "getReleaseFlux",
      signature= "Model",
      definition=function 
      (
      object 
      ){
      C=getC(object)
      times=object@times
      Atm=object@mat
      A=getFunctionDefinition(Atm)
      n=length(object@initialValues)
      rfunc=RespirationCoefficients(A)
      if (n==1) { r=matrix(ncol=n,sapply(times,rfunc))}
      else {r=t(sapply(times,rfunc))}
      R=r*C
      f=function(i){paste("ReleaseFlux",i,sep="")}
      return(R)
   }
)
setMethod(
   f= "getAccumulatedRelease",
      signature= "Model",
      definition=function 
      (object){
      times=object@times
      R=getReleaseFlux(object)
      n=ncol(R)
      if (n==1) {
          Rfuns=list(splinefun(times,R))
      }
      else{
        Rfuns=list(splinefun(times,R[,1]))
        for (i in 2:n){
            Rf=splinefun(times,R[,i])
            Rfuns=append(Rfuns,Rf)
        }
      }
      rdot=function(y,t0){
           rv=matrix(nrow=n,ncol=1)
           for (i in 1:n){
               rv[i,1]=Rfuns[[i]](t0)
           }
           return(rv)
      }
      sVmat=matrix(0,nrow=n,ncol=1)
      Y=solver(object@times,rdot,sVmat,object@solverfunc)
      f=function(i){paste("AccumulatedRelease",i,sep="")}
      return(Y)
   }
)
getSingleCol=function(x,slot_name){
    res=""
    if(slot_name=="times"){ res=getTimes(x)}
    if(slot_name=="C"){ res=getC(x)}
    if(slot_name=="ReleaseFlux"){ res=getReleaseFlux(x)}
    if(slot_name=="AccumulatedRelease"){ res=getAccumulatedRelease(x)}
    return(res)
}
setMethod("[",signature(x="Model",i="character",j="missing",drop="missing"), 
        definition=function 
        (x,i){
            n=length(i)
            df=getSingleCol(x,i[1])
            if (n>1){
                for (k in 2:n){
                    df=cbind(df,getSingleCol(x,i[k]))
                }
            }
            return(df)
        }
)
