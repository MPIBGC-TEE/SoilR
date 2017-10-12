\description{The class Model is the focal point of SoilR. 
\itemize{
\item It combines all the components that are needed to solve the
initial value problem for the pool contents.
\eqn{\vec{C}}{C=(C_1,...C_n)^t, }. 
\item It provides the single argument for the different functions 
that are available to compute various results from the solution of the initial value problem. 
See subsection \code{Methods} and the examples.)
}}
\details{The initial value problem is given by:
\itemize{
\item
the ordinary differential equation
\eqn{ \dot{\vec{C}} = \tens{A}(t) \vec{C} =\vec{I}(t)}{ d/dt C=A(t)C+I(t),}
\item
the intial Values \eqn{\vec{C}_0=\vec{C}(t_0)}{C_0=C(t_0),} 
\item 
for the times \eqn{\{t_0,....t_m\}}{{t_0,....,t_m}}.
}
In an object of class Model the components are represented as follows:
\itemize{
\item
The time-dependent matrix valued function \eqn{\vec{A}(t)}{A(t)} is represented by an object 
of a class that inherits from class \code{DecompOp} \code{\link{DecompOp-class}}. 
Such objects can be created in different ways from functions, matrices or data. 
(see the subclasses of \code{\link{DecompOp-class}} and especially their \code{Constructors} sections.  
and the \code{examples} section of this help page.
\item 
The vector-valued time-dependent function \eqn{\vec{I}(t)}{I(t)} is in SoilR represented by an object of a class that 
inherits from class InFlux \code{\link{InFlux-class}}. 
Such objects can be created from functions, constant vectors and data. 
(see the subclasses of \code{\link{InFlux-class}} and especially their \code{Constructors} sections.  
\item 
The times for which the results are computed are represented by a numeric vector.
\item 
The initial values are represented by a numeric vector 
}}
\name{Model-class}
\alias{Model-class}
\docType{class}
\section{Methods}{  \describe{
    \item{[}{\code{signature(x = "Model", i = "character", j = "missing", drop = "missing")}: ... } \code{\link{[,Model,character,missing,missing-method}}
    \item{getAccumulatedRelease}{\code{signature(object = "Model")}: ... } \code{\link{getAccumulatedRelease,Model-method}}
    \item{getC}{\code{signature(object = "Model")}: ... } \code{\link{getC,Model-method}}
    \item{getReleaseFlux}{\code{signature(object = "Model")}: ... } \code{\link{getReleaseFlux,Model-method}}
    \item{getTimes}{\code{signature(object = "Model")}: ... } \code{\link{getTimes,Model-method}}
	 }}
\title{Model}
\section{Subclasses}{\describe{
	\code{\link{Model_14-class}}\cr
}}
\section{Constructors}{	\code{\link{Model}}\cr
 Please also look at constructors of non virtual subclasses }
\examples{# examples from external files
# inst/examples/CorrectLinearModel.R CorrectLinearModel:

  # This example describes the creation and use of a Model object that 
  # is defined by time dependent functions for decomposition and influx.
  # Before we can call the constructor of the class we 
  # create its arguments explicitly. 
  # We start with the Decomposition Operator.
  # For this example we assume that we are able to describe it 
  # by explicit R functions and therefore choose the appropriate
  # sub class BoundLinDecompOp
  # of DecompOp explicitly.  (see ?'BoundLinDecompOp-class') 
  A=BoundLinDecompOp(
    ## We call the generic constructor (see ?BoundLindDcompOp) 
    ## which has a method  
    ## that takes a matrix-valued function of time as its first argument.
    function(t){
      matrix(nrow=3,ncol=3,byrow=TRUE,
         c(
           -1,    0,        0,
          0.5,   -2,        0,
            0,    1, sin(t)-1 
        )
      )    
    },
    ## The other two arguments describe the time interval where the 
    ## function is valid (the domain of the function)
    ## The interval will be checked against the domain of the InFlux
    ## argument of Model and against its 't' argument to avoid 
    ## invalid computations outside the domain. 
    ##  Inf and -Inf are possible values. 
    starttime=0,
    endtime=20
  )  
  I=BoundInFlux(
     ## The first argument is a vector-valued function of time
     function(t){
       matrix(nrow=3,ncol=1,byrow=TRUE,
           c(-1,    0,    0)
       )
     },
     ## The other two arguments describe the time interval where the 
     ## function is valid (the domain of the function)
     starttime=0,
     endtime=40
  )
  ## No we specify the points in time where we want to compute results
  t_start=0 
  t_end=10 
  tn=50
  timestep <- (t_end-t_start)/tn 
  times <- seq(t_start,t_end,timestep) 
  ## and the start values
  sv=c(0,0,0)
  mod=Model(t=times,A,sv,I)

  ## No we use the model to compute some results
  getC(mod)
  getReleaseFlux(mod)
  #also look at the methods section of Model-class 
}
