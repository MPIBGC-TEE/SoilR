\name{Model}
\alias{Model}
\usage{Model(t,
A,
ivList,
inputFluxes,
solverfunc=deSolve.lsoda.wrapper,
pass=FALSE)}
\examples{# examples from external files
# inst/tests/requireSoilR/runit.all.possible.Model.arguments.R test.all.possible.Model.arguments:

  # This example shows different kinds of arguments to the function Model.
  # The model objects we will build will share some common features.
  #  - two pools 
  #  - initial values 

       iv<-  c(5,6)

  #  - times 

       times <- seq(1,10,by=0.1)

  # The other parameters A and inputFluxes will be different
  # The function Model will transform these arguments 
  # into objects of the classes required by the internal constructor.
  # This leads to a number of possible argument types. 
  # We demonstrate some of the possibilities here.
  # Let us first look at the choeices for argument 'A'.

  A.matrix              <- matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2))
  A.matrix.func         <- function(t){
                            matrix(nrow=2,
                              byrow=TRUE,
                              c(-0.1,0,0,-0.2*(sin(t)+2)))}
  A.list.array          <- example.Time3DArrayList()
  A.list.mat.list       <- example.nestedTime2DMatrixList()
  # Model will convert these to the classes it uses internally but 
  # you can also create these object explicitly, which in some situations
  # gives you more control.
  A.TimeMap             <- TimeMap(example.nestedTime2DMatrixList())
  A.ConstlinDecompOp    <- example.ConstlinDecompOpFromMatrix()
  A.BoundLinDecompOp    <- example.2DBoundLinDecompOpFromFunction()
  A.UnBoundLinDecompOp  <- example.2DUnBoundLinDecompOpFromFunction()
  
  
  # Since "Model" will call "GeneralInFlux" on its "inputFluxes" 
  # argument there are again different choices
  
  I.vec          =c(5,6) 
  I.BoundInFlux  =SoilR:::example.2DBoundInFluxFromFunction()
  I.ConstInFlux  =SoilR:::example.2DConstInFluxFromVector()
    # We can build a lot of  models from the possible combinations
     
     Model(t=times,A=A.matrix.func,ivList=iv,inputFluxes=I.vec) 

  models <- list(
    Model(t=times,A=A.matrix,ivList=iv,inputFluxes=I.vec)
    ,
    Model(t=times,A=A.matrix,ivList=iv,inputFluxes=I.BoundInFlux)
    ,
    Model(t=times,A=A.matrix,ivList=iv,inputFluxes=I.ConstInFlux)
    ,

    Model(t=times,A=A.matrix.func,ivList=iv,inputFluxes=I.vec)
    ,
    Model(t=times,A=A.matrix.func,ivList=iv,inputFluxes=I.BoundInFlux)
    ,
    Model(t=times,A=A.matrix.func,ivList=iv,inputFluxes=I.ConstInFlux)
    ,

    Model(t=times,A=A.list.mat.list,ivList=iv,inputFluxes=I.vec)
    ,
    Model(t=times,A=A.list.mat,ivList=iv,inputFluxes=I.BoundInFlux)
    ,
    Model(t=times,A=A.list.mat,ivList=iv,inputFluxes=I.ConstInFlux)
    ,

    Model(t=times,A=A.list.array,ivList=iv,inputFluxes=I.vec)
    ,
    Model(t=times,A=A.list.array,ivList=iv,inputFluxes=I.BoundInFlux)
    ,
    Model(t=times,A=A.list.array,ivList=iv,inputFluxes=I.ConstInFlux)
    ,

    Model(t=times,A=A.TimeMap,ivList=iv,inputFluxes=I.vec)
    ,
    Model(t=times,A=A.TimeMap,ivList=iv,inputFluxes=I.BoundInFlux)
    ,
    Model(t=times,A=A.TimeMap,ivList=iv,inputFluxes=I.ConstInFlux)
    ,

    Model(t=times,A=A.ConstlinDecompOp,ivList=iv,inputFluxes=I.vec)
    ,
    Model(t=times,A=A.ConstlinDecompOp,ivList=iv,inputFluxes=I.BoundInFlux)
    ,
    Model(t=times,A=A.ConstlinDecompOp,ivList=iv,inputFluxes=I.ConstInFlux)
    ,

    Model(t=times,A=A.ConstlinDecompOp,ivList=iv,inputFluxes=I.vec)
    ,
    Model(t=times,A=A.ConstlinDecompOp,ivList=iv,inputFluxes=I.BoundInFlux)
    ,
    Model(t=times,A=A.ConstlinDecompOp,ivList=iv,inputFluxes=I.ConstInFlux)
    )
  # lets check that we can compute something# 
  lapply(models,getC)
  
}
\arguments{
\item{t}{A vector containing the points in time where the solution is sought.}
\item{A}{something that can be converted by \link{GeneralDecompOp} to any of the available subclasses of \code{\link{DecompOp-class}}.}
\item{ivList}{A numeric vector containing the initial amount of carbon for the n pools. The length of this vector is equal to the number of pools. This is checked by an internal  function.}
\item{inputFluxes}{something that can be converted by \link{GeneralInFlux} to any of the available subclasses of \link{InFlux-class}.}
\item{solverfunc}{The function used to actually solve the ODE system. The default is \code{\link{deSolve.lsoda.wrapper}} but you can also provide your own function that provides the same interface.}
\item{pass}{Forces the constructor to create the model even if it does not pass internal sanity checks}
}
\description{This function creates an object of class \link{Model-class}, 
The arguments can be given in different form as long as they can 
be converted to the necessary internal building blocks. 
(See the links)}
\details{This function \code{Model} wraps the internal constructor of class \link{Model-class}. The internal constructor requires the argument \code{A} to be of class \link{DecompOp-class} and argument \code{inputFluxes} to be of  class \link{InFlux-class}.
Before calling the internal constructor \code{Model} calls \link{GeneralDecompOp} on its argument \code{A} and  \link{GeneralInFlux} on its argument \code{inputFluxes} to convert them into
the required classes.
Both are generic functions. Follow the links to see for which kind of inputs conversion methods are available.
The attempted conversion allows great flexibility with respect to arguments and independence from the actual implementation.
However if your code uses the wrong argument the error will most
likely occur in the delegate functions. 
If this happens use \code{traceback()} to see which function was called
and try to call the constructor of the desired subclass 
explicitly with your arguments. 
The subclasses are linked in the class documentation \link{DecompOp-class} or \link{InFlux-class} respectively.

Note also that this function checks its arguments quite elaborately 
and tries to detect accidental unreasonable combinations, 
especially concerning two kinds of error.
\enumerate{
\item 
unintended extrapolation of time series data
\item
violoations of massbalance by the DecompOp argument.
}

SoilR has a lot of unit tests which are installed i
with the package and are sometimes instructive as examples. 
To see example scenarios for parameter check look at:
\Sexpr[echo=TRUE,keep.source=TRUE,stage=render,results=verbatim]{system.file('tests','runit.correctness_of_Model.R',package='SoilR')}}
\seealso{This function is called by many of the \link{predefinedModels}. \cr 
Package functions called in the examples:\cr
\code{\link{example.2DBoundInFluxFromFunction}},\cr
\code{\link{example.2DConstInFluxFromVector}},\cr
\code{\link{example.ConstlinDecompOpFromMatrix}},\cr
\code{\link{example.2DBoundInFluxFromFunction}},\cr
\code{\link{example.TimeMapFromArray}}}
\title{Constructor for class \link{Model-class} }
\value{An object of class \link{Model-class} that can be queried by many methods 
to be found there.}
