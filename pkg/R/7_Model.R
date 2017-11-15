#
# vim:set ff=unix expandtab ts=2 sw=2:
correctnessOfModel <- function #check for unreasonable input parameters
### The parameters used by the function \code{\link{GeneralModel}} in SoilR have a biological meaning, and therefore cannot be arbitrary.
### This functions tests some of the obvious constraints of the general model. 
### Up to now these are:
### 1) The compatibility of the decomposition rates and the transport parameters to and from other pools, i.e. 
### the column-wise sum of the elements cannot be negative. Otherwise this would create negative values of respiration, which are not biologically meaningful.
### 2) The compatibility of the time ranges of the supplied functions 

(object)
{   
    times=object@times
    Atm=object@mat
    ivList=object@initialValues
    InFluxes=object@inputFluxes
    #first we check the dimensions
    A=getFunctionDefinition(Atm)
    na=nrow(A(0))
    #compute the respiration coefficients as funtions of time
    rcoeffs=RespirationCoefficients(A)
    r=sapply(times,rcoeffs)
    #mark the negative respirations (which will trigger the refusal of the matrix )
    truthv=sapply(r,is.negative)
    #find the bad columns 
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
    haveALook="Have look at the object containing  A(t) or the data it is created from"
    if (t_min<tA_min) {
        stop(
          simpleError(
            paste(
              "You ordered a timeinterval that starts earlier than the interval your matrix valued function A(t) is defined for. \n ",
              haveALook
            )
          )
        )
    }
    if (t_max>tA_max) {
        stop(
          simpleError(
            paste(
              "You ordered a timeinterval that ends later than the interval your matrix valued function A(t) is defined for. \n ",
              haveALook
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
   ### the function returns True if the argumente is negative
   return(number<0)
}
### The class Model is the focal point of SoilR. 
### \itemize{
### \item It combines all the components that are needed to solve the
### initial value problem for the pool contents.
### \eqn{\vec{C}}{C=(C_1,...C_n)^t, }. 
### 
### \item It provides the single argument for the different functions 
### that are available to compute various results from the solution of the initial value problem. 
### (See subsection \code{Methods} and the examples.)
###}
setClass(# Model
   Class="Model",
   representation=representation(
        times="numeric"
        ,
        mat="DecompOp"
        ,
        initialValues="numeric"
        ,
        inputFluxes="InFlux"
        ,
        solverfunc="function"
   ) , 
   validity=correctnessOfModel #set the validating function
    ##details<<
    ## The initial value problem is given by:
    ## \itemize{
    ##   \item
    ##     the ordinary differential equation
    ##     \eqn{ \dot{\vec{C}} = \tens{A}(t) \vec{C} =\vec{I}(t)}{ d/dt C=A(t)C+I(t),}
    ##   \item
    ##     the intial Values \eqn{\vec{C}_0=\vec{C}(t_0)}{C_0=C(t_0),} 
    ##   \item 
    ##     for the times \eqn{\{t_0,....t_m\}}{{t_0,....,t_m}}.
    ## }
    ## In an object of class Model the components are represented as follows:
    ## \itemize{
    ##   \item
    ##   The time-dependent matrix valued function \eqn{\vec{A}(t)}{A(t)} is represented by an object 
    ##   of a class that inherits from class  \code{\linkS4class{DecompOp}}. 
    ##   Such objects can be created in different ways from functions, matrices or data. 
    ##   (see the subclasses of \code{\linkS4class{DecompOp}} and especially their \code{Constructors} sections.  
    ##   and the \code{examples} section of this help page.
    ##   \item 
    ##   The vector-valued time-dependent function \eqn{\vec{I}(t)}{I(t)} is in SoilR represented by an object of a class that 
    ##   inherits from class InFlux \code{\linkS4class{InFlux}}. 
    ##   Such objects can be created from functions, constant vectors and data. 
    ##   (see the subclasses of \code{\linkS4class{InFlux}} and especially their \code{Constructors} sections.  
    ##   \item 
    ##   The times for which the results are computed are represented by a numeric vector.
    ##   \item 
    ##   The initial values are represented by a numeric vector 
    ## }
   ##exampleFunctionsFromFiles<< 
   ## inst/examples/ModelExamples.R CorrectNonautonomousLinearModelExplicit 
)


#------------------------------------------------------------------------------------
setMethod(
    f="initialize",
    signature="Model",
    definition=function #internal constructor for Model objects
    ### Note that we encourage the use of more convienient constructors for the creation of model objects.
    ### Since this method is tightly coupled to the internal implementation of the class it is much more likely to change in the future than the other constructors, which can be kept stable much more easily in the future and are therefore encouraged for user code. 
    ### This method implements R's initialize generic for objects of class \code{Model} 
    ### It is called whenever a new object of this class is created by a call to \code{new} with the first argument \code{Model}.
    ### It performs some sanity checks of its arguments and in case those tests pass returns an object of class \code{Model}. 
    ### The checks can be turned off.( see arguments)

    ##details<<  Due to the mechanism of S4 object initialization (package "methods")
    ## \code{new} always calls \code{initialize}. 
    ## (see the help pages for initialize and initialize-methods for details)  
    
    ## All other constructors of class \code{Model} have to call \code{new("Model,..) at some point and thus call this method indirectly. 
    ## Accordingly this method is the place to perform checks all objects of class \code{Model} should pass.
    ## Some of those checks are explained in the examples below.
    ## In some (rare) circumstances it might be necessary to override the checks and force the object to be created 
    ## although it does not seem meaningfull to the internal sanity checks. 
    ## You can use the "pass" argument to enforce this.
    (
        .Object,
        times=c(0,1),
        mat=ConstLinDecompOp(matrix(nrow=1,ncol=1,0)), ##<< A decomposition Operator of some kind 
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
         if (class(mat)=="TimeMap"){
          warning(TimeMapWarningOperators())
            # cast
            mat <- BoundLinDecompOp(mat)
         }
        .Object@times=times
        .Object@mat=mat
        .Object@initialValues=initialValues
         if (class(inputFluxes)=="TimeMap"){
          warning(TimeMapWarningInFluxes())
            # cast
            inputFluxes<- BoundInFlux(inputFluxes)
          }
        .Object@inputFluxes=inputFluxes
        .Object@solverfunc=solverfunc
        #if (pass==FALSE) validObject(.Object) #call of the ispector if not explicitly disabled
        if (pass==FALSE) correctnessOfModel(.Object) #call of the ispector if not explicitly disabled
        return(.Object)
        ### an Object of class Model
    }
)

#------------------------------------------------------------------------------------
#                               Constructors
#------------------------------------------------------------------------------------
#setMethod(f="Model",
#  signature=c(
#    "numeric",
#    "ANY",
#    "numeric"#,
#    #"ANY"
#  ),
#  definition=function #general  constructor for class Model
Model <- function #Constructor for class \linkS4class{Model} 
  ### This function creates an object of class \linkS4class{Model}, 
  ### The arguments can be given in different form as long as they can 
  ### be converted to the necessary internal building blocks. 
  ### (See the links)

  (t,			##<< A vector containing the points in time where the solution is sought.
   A,			##<< something that can be converted by \link{GeneralDecompOp} to any of the available subclasses of \code{\linkS4class{DecompOp}}. 
   ivList,		##<< A numeric vector containing the initial amount of carbon for the n pools. The length of this vector is equal to the number of pools. This is checked by an internal  function. 
   inputFluxes, ##<<  something that can be converted by \link{GeneralInFlux} to any of the available subclasses of \linkS4class{InFlux}.
   solverfunc=deSolve.lsoda.wrapper,		##<< The function used to actually solve the ODE system. The default is \code{\link{deSolve.lsoda.wrapper}} but you can also provide your own function that the same interface. 
   pass=FALSE  ##<< Forces the constructor to create the model even if it does not pass internal sanity checks  
   )
  {
  ##details<< This function \code{Model} wraps the internal constructor of class \linkS4class{Model}. The internal constructor requires the argument \code{A} to be of class \linkS4class{DecompOp} and argument \code{inputFluxes} to be of  class \linkS4class{InFlux}.
  ## Before calling the internal constructor \code{Model} calls \link{GeneralDecompOp} on its argument \code{A} and  \link{GeneralInFlux} on its argument \code{inputFluxes} to convert them into
  ## the required classes.
  ## Both are generic functions. Follow the links to see for which kind of inputs conversion methods are available.
  ## The attempted conversion allows great flexibility with respect to arguments and independence from the actual implementation.
  ## However if your code uses the wrong argument the error will most
  ## likely occur in the delegate functions. 
  ## If this happens analyse the errormassage (or use \code{traceback()}) to see which function was called
  ## and try to call the constructor of the desired subclass 
  ## explicitly with your arguments. 
  ## The subclasses are linked in the class documentation \linkS4class{DecompOp} or \linkS4class{InFlux} respectively.
  ##
  ## Note also that this function checks its arguments quite elaborately 
  ## and tries to detect accidental unreasonable combinations, 
  ## especially concerning two kinds of errors.
  ## \enumerate{
  ## \item 
  ##  unintended extrapolation of time series data
  ## \item
  ##  violoations of massbalance by the DecompOp argument.
  ##}
  ## 
  ## SoilR has a lot of unit tests which are installed i
  ## with the package and are sometimes instructive as examples. 
  ## To see example scenarios for parameter check look at:
  ## \Sexpr[echo=TRUE,keep.source=TRUE,stage=render,results=verbatim]{system.file('tests','runit.correctness_of_Model.R',package='SoilR')}

  
  
     obj=new(Class="Model",t,GeneralDecompOp(A),ivList,GeneralInFlux(inputFluxes),solverfunc,pass)
     return(obj)
     ### An object of class \linkS4class{Model} that can be queried by many methods 
     ### to be found there.
     ##seealso<< This function is called by many of the \link{predefinedModels}. \cr 
     ##Package functions called in the examples:\cr
     ##\code{\link{example.2DInFlux.Args}},\cr
     ##\code{\link{example.2DGeneralDecompOpArgs}},\cr

     ##exampleFunctionsFromFiles<< 
     ## inst/tests/requireSoilR/runit.all.possible.Model.arguments.R test.all.possible.Model.arguments
  }
#)
     ## #inst/examples/ModelExamples.R CorrectNonautonomousLinearModelExplicit 
#------------------------------------------------------------------------------------
#                               Methods
#------------------------------------------------------------------------------------
setMethod(
   f= "plot",
      signature(x="Model"),
      definition=function # not yet implemented method to plot an overview of several inputs and outpust of a model 
      ### This function is a stub
      (x){
      # It only starts the thing ...    
      plot(getTimes(x),getC(x)[,1])
   }
)
#------------------------------------------------------------------------------------
setMethod(
   f= "print",
      signature(x="Model"),
      definition=function# not yet implemented method to print an overview of several inputs and outpust of a model  
      ### This function is a stub
      (x){
      # It only starts the thing ...    
   }
)
#------------------------------------------------------------------------------------
setMethod(
   f= "summary",
      signature(object="Model"),
      definition=function 
      ### This function is a stub
      (object){
      # It only starts the thing ...    
#      print("Hi there, I am the method summarize for model objects. 
#            I summarize everything....")
#      print(getC(object)[,1])
   }
)
#------------------------------------------------------------------------------------
setMethod(
   f= "show",
      signature(object="Model"),
      definition=function
      ### This function is a stub
      (object){
      # It only starts the thing ...    
#      print("Hi there I am the method show for model objects")
#      print(getC(object)[,1])
   }
)

#------------------------------------------------------------------------------------
setMethod(
   f= "getTimes",
      signature= "Model",
      definition=function(object){
      ### This functions extracts the times argument from an argument of class Model
         times=matrix(ncol=1,object@times)
         colnames(times)="times"
      return(times)
   }
)
#------------------------------------------------------------------------------------
setMethod(
   f= "getC",
      signature= "Model",
      definition=# get Carbon stocks as function of time
      function(object){
      ### This function computes the value for C for each time and pool. 
      ns=length(object@initialValues)
      Atm=object@mat
      #print(Atm)
      A=getFunctionDefinition(Atm)
      #print(A)
      itm=object@inputFluxes
      input=getFunctionDefinition(itm)
      #print(input)
      ydot=NpYdot(A,input)
      #print(ydot)
      sVmat=matrix(object@initialValues,nrow=ns,ncol=1)
      Y=solver(object@times,ydot,sVmat,object@solverfunc) 
      
      #print(Y)
      ### A matrix. Every column represents a pool and every row a point in time
      f=function(i){paste("C",i,sep="")}
      #colnames(Y)=sapply((1:ncol(Y)),f)
      return(Y)
      ##value<< A matrix with m columns representing the number of pools, and n rows representing the times as specified by the argument
      ##\code{t} in \code{\link{Model}},\code{\link{GeneralModel}} or another model creating function.
      ##details<< This function takes a Model object, which represents a system of ODEs of the form 
      ##\deqn{\frac{d \mathbf{C}(t)}{dt} = \mathbf{I}(t) + \mathbf{A}(t) \mathbf{C}(t)}{dC(t)/dt = I(t) + A(t)C(t)} 
     ##and solves the system for \eqn{\mathbf{C}(t)}{C(t)}. The numerical solver used can be specified in the constructor of the Model class
    ## e.g. \code{\link{Model}}, \code{\link{GeneralModel}}.
	  ##seealso<< See examples in \code{\link{GeneralModel}}, \code{\link{GeneralModel_14}}, \code{\link{TwopParallelModel}}, 
    ## \code{\link{TwopSeriesModel}}, \code{\link{TwopFeedbackModel}}, etc.
   }
)
#------------------------------------------------------------------------------------
setMethod(
   f= "getReleaseFlux",
      signature= "Model",
      definition=function # get the release rate for all pools 
      ### The method computes the release of carbon per time for all points in time 
      ### specified in the Model objects time slot.
      (
      object ##<< an object of class Model
      ## created by a call to a constructor e.g. \code{\link{Model}}, 
      ## \code{\link{GeneralModel}}or other model creating functions.
      ){
      C=getC(object)
      times=object@times
      Atm=object@mat
      A=getFunctionDefinition(Atm)
      n=length(object@initialValues)
      rfunc=RespirationCoefficients(A)
      #rfunc is vector valued function of time
      if (n==1) { r=matrix(ncol=n,sapply(times,rfunc))}
      else {r=t(sapply(times,rfunc))}
      R=r*C
      f=function(i){paste("ReleaseFlux",i,sep="")}
      #colnames(R)=sapply((1:ncol(R)),f)
      return(R)
	    ##value<< A n x m matrix of release fluxes with m columns representing the number of pools, and n rows representing the values for each time step as specified by the argument
	    ## \code{t} in \code{\link{Model}}, \code{\link{GeneralModel}} or another model creating function.
	    ##details<< This function takes a Model object, which represents a system of ODEs 
	    ## \deqn{\frac{d \mathbf{C}(t)}{dt} = \mathbf{I}(t) + \mathbf{A}(t) \mathbf{C}(t)}{dC(t)/dt = I(t) + A(t)C(t)} 
	    ## solves the system for \eqn{\mathbf{C}(t)}{C(t)}, calculates the release coefficients \eqn{\mathbf{R}(t)}{R(t)}, 
      ## and computes the release flux as \eqn{\mathbf{R}(t) \mathbf{C}(t)}{R(t) C(t)}.
      ## The numerical solver used can be specified in the model creating functions like e.g. \code{\link{Model}}.
   }
)
#------------------------------------------------------------------------------------
setMethod(
   f= "getAccumulatedRelease",
      signature= "Model",
      definition=function # time integrals of release fluxes per pool
      ### The method integrates the release flux of every pool for all times in the  interval specified by the model definition.

      (object){
      times=object@times
      R=getReleaseFlux(object)
      n=ncol(R)
      #transform the array to a list of functions of time by
      #intepolating it with splines
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
      #test=Rfuns[[1]]
      #now we can construct the derivative of the respiration as function of time
      #as needed by the ode solver
      rdot=function(y,t0){
           # the simples possible case for an ode solver is that the ode is
           # just an integral and does not depend on the value but only on t
           # This is the case here
           rv=matrix(nrow=n,ncol=1)
           for (i in 1:n){
               #print(Rfuns[i])
               rv[i,1]=Rfuns[[i]](t0)
           }
           return(rv)
      }
      sVmat=matrix(0,nrow=n,ncol=1)
      Y=solver(object@times,rdot,sVmat,object@solverfunc)
      f=function(i){paste("AccumulatedRelease",i,sep="")}
      #colnames(Y)=sapply((1:ncol(Y)),f)
      return(Y)
      ### a matrix
   }
)
## overload the [] operator
#------------------------------------------------------------------------------------
getSingleCol=function(x,slot_name){
    res=""
    #print(paste(sep="",">",slot_name,"<"))
    if(slot_name=="times"){ res=getTimes(x)}
    if(slot_name=="C"){ res=getC(x)}
    if(slot_name=="ReleaseFlux"){ res=getReleaseFlux(x)}
    if(slot_name=="AccumulatedRelease"){ res=getAccumulatedRelease(x)}
    #if(res==""){stop(paste("The slot",slot_name,"is not defined"))}
    return(res)
}
setMethod("[",signature(x="Model",i="character",j="missing",drop="missing"), #since [] is a already defined generic the names of the arguments are not arbitrary 
        definition=function # overload the [ ] operator for models and character vector
        ### This method is experimental and might change. User code should at the moment not rely on it. It overloads the [] operator for Model objects and acts as an alternative
        ### to the get methods
        ### so m["times"] is equivalent to getTimes(m), m["C"] to getC(m) and so on.

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
## overload the [[ operator
#------------------------------------------------------------------------------------
#setMethod("[[",signature(x="Model"), #since [[ is a already defined generic the names of the arguments are not arbitrary 
#        definition=function #overloads the $ operator 
#        ### The operator internally calls one of the methods getC,getReleaseFlux,getAccumulatedRelease or getTimes
#        (x,name){
#            #return(x[name])
#            return(singleCol(x,name))
#        }
#)
#
#------------------------------------------------------------------------------------
# do not  overload the $ operator since it is used in R5 classes as method send

