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
