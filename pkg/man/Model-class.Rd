\description{The Model class is the focal point of SoilR. 
\itemize{
\item An object of this class (see also the section \code{Subclasses})  collects all components that are needed to run a simulation. 
These components of a model can be provided in many different ways.  (see the links under subsection \code{Constructors for this class or any of ist sublasses} for functions that create \code{Model} objects.)
\item Different functions are available to compute different results for the simulation. (See subsection \code{Methods}.)
}}
\details{The class is essentian to  decouple implementation details from the user interface. }
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
\section{Examples}{# examples from external files
\preformatted{
# inst/examples/CorrectLinearModel.R CorrectLinearModel:

  ## We first specify the points in time where we want to compute results
  t_start=0 
  t_end=10 
  tn=50
  timestep <- (t_end-t_start)/tn 
  times <- seq(t_start,t_end,timestep) 
  A=BoundLinDecompOp(
    ## The first argument is a matrix-valued function of time
    function(t){
      matrix(nrow=3,ncol=3,byrow=TRUE,
         c(
           -1,    0,        0,
          0.5,   -2,        0,
            0,    1, sin(t)-1 
        )
      )    
    },
    ## The other two arguments describe the time interval where the function is valid (the domain of the function)
    ## This interval must include all times specified in the \code{times} argument of the model. You can also use 
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
     ## The other two arguments describe the time interval where the function is valid (the domain of the function)
     t_start,
     t_end
  )
  res=Model(times,A,c(0,0,0),I)

}}
