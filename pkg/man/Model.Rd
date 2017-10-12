\name{Model}
\alias{Model}
\usage{Model(t,
A,
ivList,
inputFluxes,
solverfunc=deSolve.lsoda.wrapper,
pass=FALSE)}
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
\arguments{
\item{t}{A vector containing the points in time where the solution is sought.}
\item{A}{something that can be converted by \link{GeneralDecompOp} to any of the available subclasses of \code{\link{DecompOp-class}}.}
\item{ivList}{A numeric vector containing the initial amount of carbon for the n pools. The length of this vector is equal to the number of pools .This is checked by an internal  function.}
\item{inputFluxes}{something that can be converted by \link{GeneralInFlux} to any of the available subclasses of \link{InFlux-class}.}
\item{solverfunc}{The function used to actually solve the ODE system. This can be \code{\link{deSolve.lsoda.wrapper}} or any other user provided function with the same interface.}
\item{pass}{Forces the constructor to create the model even if it does not pass internal sanity checks}
}
\description{This function creates an object of class \link{Model-class}, 
The arguments can be given in different form as long as they can 
be converted to the necessary internal building blocks. 
(See the links)}
\details{The internal constructor of class \link{Model-class} requires the argument \code{A} to be of class \link{DecompOp-class} and argument \code{inputFluxes} to be of  class \link{InFlux-class}.
Before calling the internal constructor this function calls \link{GeneralDecompOp} on its argument \code{A} and  \link{GeneralInFlux} on its argument \code{inputFluxes} to convert them into
the required classes.
Both are generic functions. Follow the links to see for which kind of inputs conversion methods are available.
The attempted conversion allows great flexibility with respect to arguments and independence from the actual implementation.
However if your code uses the wrong argument the error will most
likely occur in the delegate functions. 
If this happens use \code{traceback()} to see which function was called
and try to call the constructor of the desired subclass 
explicitly with your arguments. 
The subclasses are linked in the class documentation \link{DecompOp-class} or \link{InFlux-class} respectively.}
\seealso{This function is called by many of the \link{predefinedModels} }
\title{Constructor for class \link{Model-class} }
\value{An object of class \link{Model-class} that can be queried by many methods 
to be found there.}
