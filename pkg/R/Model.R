


#' @template Initialize-Boiler-Plate
#' 
#' @param .Object no manual documentation
#' @param times no manual documentation
#' @param mat no manual documentation
#' @param initialValues no manual documentation
#' @param inputFluxes no manual documentation
#' @param solverfunc no manual documentation
#' @param pass no manual documentation
#' @autocomment These comments were created by the auto_comment_roclet by
#' inspection of the code.  You can use the "update_auto_comment_roclet" to
#' automatically adapt them to changes in the source code. This will remove
#' `@param` tags for parameters that are no longer present in the source code
#' and add `@param` tags with a default description for yet undocumented
#' parameters.  If you remove this `@autocomment` tag your comments will no
#' longer be touched by the "update_autocomment_roclet".
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
#' \link{InFluxes} on its argument \code{inputFluxes} to convert them into
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
#' @param inputFluxes something that can be converted by \link{InFluxes}
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
#' \code{\link{example.2DInFluxes.Args}},\cr
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
#'   # Since "Model" will call "InFluxes" on its "inputFluxes" 
#'   # argument there are again different choices
#'   # we have included a function in SoilR that produces 2D examples
#'   
#'   possibleInfluxes <- example.2DInFluxes.Args()
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
Model <-function(t,			
   A,			
   ivList,		
   inputFluxes, 
   solverfunc=deSolve.lsoda.wrapper,		
   pass=FALSE  
)
{   
     obj=new(
        Class="Model"
        ,t
        ,GeneralDecompOp(A)
        ,ivList
        ,InFluxes(inputFluxes,numberOfPools=length(ivList))
        ,solverfunc
        ,pass
        )
     return(obj)
}




#' Extract the InFluxes as provided during creation of the model
#'
#' Since the influxes had to be provided to create the model this method
#' yields no new information that can not be obtained simpler.  
#' It is usually called internally by other functions. 
#' @template Model-param
#' @autocomment 
setMethod(
   f= "getInFluxes",
      signature(object="Model"),
      definition=function(object){
      object@inputFluxes
   }
)



#' Create an overview plot 
#' 
#' The method solves the model and plots the solutions
#' It is intended to provide a quick overview.
#' @param x The model (run) the results of which are plotted
#' @autocomment 
setMethod(
   f= "plot",
   signature(x="Model"),
   definition=function (x){
     plot(getTimes(x),getC(x)[,1])
   }
)

## automatic title
##  
## @param x no manual documentation
## @autocomment 
#setMethod(
#   f= "print",
#      signature(x="Model"),
#      definition=function
#      (x){
#   }
#)



## automatic title
## 
## @param object no manual documentation
## @autocomment 
#setMethod(
#   f= "summary",
#      signature(object="Model"),
#      definition=function 
#      (object){
#   }
#)



## Transform the model to a string
## 
## @template Model-param
## @autocomment 
#setMethod(
#   f= "show",
#      signature(object="Model"),
#      definition=function
#      (object){
#   }
#)



#' Extract the Compartmental Operator
#' 
#' The method is usually used internally by other methods operating on 
#' models. The information it yields has either been provided by the user in
#' creating the modelrun or can be obtained by directly transforming the
#' arguments that were used.
#' @template Model-param
#' @return The actual class of the result can vary. 
#' It will be a subclass of \code{\linkS4class{DecompOp}}. The information contained
#' in these objects is equivalent to the "Compartmental Matrix" of 
#' the model.
#' In the general case of a nonautonomuous nonlinear Model this is a matrix
#' valued function of the pool contents and time. 
setMethod(
   f= "getDecompOp",
      signature= "Model",
      definition=function(object){
      return(object@mat)
   }
)



#' Extract the times vector
#' 
#' Since the \code{times} had to be provided to create the model this method
#' yields no new information. 
#' It is usually called internally by other functions that deal with models. 
#' @template Model-param
#' @autocomment 
setMethod(
   f= "getTimes",
      signature= "Model",
      definition=function(object){
         times=matrix(ncol=1,object@times)
         colnames(times)="times"
      return(times)
   }
)



#' Derivative of the state variables as function 
# 
#' For non-linear models or models with state dependent insfluxes 
#' the returned function is a true function of state and time
#' For linear models with state indendent influxes the returned 
#' function is in fact a function of time only.
#' 
#' @param object no manual documentation
#' @return A function \eqn{f(t)}{f(t)}
#'
#' @autocomment 
setMethod(
    f='getRightHandSideOfODE'
    ,signature= "Model"
    ,definition= function(object){
        ns=length(object@initialValues)
        Atm=object@mat
        A=getFunctionDefinition(Atm)
        itm=object@inputFluxes
        input=getFunctionDefinition(itm)
        ydot=NpYdot(A,input)
    }
)



#' Pool Contents for all times
#' 
#' @template getC-description-common
#' @template Model-param
#' @template PoolWiseReturnMatrix
#' @autocomment 
setMethod(
    f= "getC"
    ,signature= "Model"
    ,definition= function(object){
        ns=length(object@initialValues)
        ydot=getRightHandSideOfODE(object)
        sVmat=matrix(object@initialValues,nrow=ns,ncol=1)
        Y=solver(object@times,ydot,sVmat,object@solverfunc) 
        #f=function(i){paste("C",i,sep="")}
        return(Y)
   }
)



#' The release fluxes \eqn{\frac{[content]}{[time]}}{[content]/[time]} for all pools.
#' 
#' @template Model-param
#' @template PoolWiseReturnMatrix
#' @autocomment 
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
      l=sapply(times,rfunc)
      if (n==1) { r=matrix(ncol=n,l)}
      else {r=t(l)}
      R=r*C
      return(R)
   }
)



#' Compute the time integral of the relaese fluxes for all times
#' 
#' @template Model-param
#' @autocomment 
#' @template PoolWiseReturnMatrix
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



#' Experimentally overloaded index operator 
#' 
#' The method provides shortcuts and a unified interface to some of the
#' methods that can be applied to a model.
#' For a given model `M` the code `M['C'] is equivalent to `getC(M)` and
#' `M['ReleaseFlux']` is equivalent to `getReleaseFlux(M)`
#' `M['AccumulatedRelease']` is equivalent to `getAccumulatedRelease(M)`
#' @param x no manual documentation
#' @param i no manual documentation
#' @autocomment 
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
