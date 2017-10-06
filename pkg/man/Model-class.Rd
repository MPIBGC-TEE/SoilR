\details{The class is essentian to  decouple implementation details from the user interface. }
\exampleFunctionsFromFiles{inst/examples/CorrectLinearModel.R CorrectLinearModel }
\title{Model}
\description{The Model class is the focal point of SoilR. 
\itemize{
\item An object of this class (or one of its subclasses) collects all components that are needed to run a simulation. (See the section \code{Subclasses}.)\n
These components of a model can be provided in many different ways.  (see the links under subsection \code{Constructors for this class or any of ist sublasses} for functions that create \code{Model} objects.)
\item Different functions are available to compute different results for the simulation. (See subsection \code{Methods}.)
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
\section{Subclasses}{\describe{
	\code{\link{Model_14-class}}\cr
}}
\section{Constructors}{	\code{\link{Model}}\cr
 Please also look at constructors of non virtual subclasses }
\examples{# examples from external files
# inst/examples/CorrectLinearModel.R CorrectLinearModel:

t_start=0
t_end=10
tn=50
timestep=(t_end-t_start)/tn
t=seq(t_start,t_end,timestep)
A=BoundLinDecompOp(
function(times){
matrix(nrow=3,ncol=3,byrow=TRUE,
c(-1,    0,    0,
0.5,   -2,    0,
0,    1, -0.5)
)
},
t_start,
t_end
)
I=BoundInFlux(
function(times){
matrix(nrow=3,ncol=1,byrow=TRUE,
c(-1,    0,    0)
)
},
t_start,
t_end
)
res=Model(t,A,c(0,0,0),I)
}
