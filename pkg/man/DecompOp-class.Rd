\description{The decomposition operator is a necessary ingredient of any Model.
Very generally it describes the fluxes between the pools and to the exterior 
as functions of time and the pool contents. 
SoilR arranges different  decomposition operators into  different classes, 
to determine which computations can be performed with them (which methods can be called on them).
The simplest and least general decomposition operator is \code{\link{ConstLinDecompOp-class}}
which can be created from a constant reservoir matrix. 
Since it is the least general (most specific) the additional information can be used to compute more 
and more specific results (more methods)
not available for the more abstract sub classes (for instance \code{\link{BoundLinDecompOp-class}} 
that are necessary to model the more general situations where the decomposition and 
transfer rates are functions of time.
The different decomposition operators are created by class-specific functions, 
called constructors. (see the constructors section of this help page)}
\name{DecompOp-class}
\alias{DecompOp-class}
\docType{class}
\section{Methods}{  \describe{
    \item{GeneralDecompOp}{\code{signature(object = "DecompOp")}: ... } \code{\link{GeneralDecompOp,DecompOp-method}}
	 }}
\title{decomposition operator }
\section{Subclasses}{\describe{
	\code{\link{UnBoundLinDecompOp-class}}\cr
	\code{\link{BoundLinDecompOp-class}}\cr
	\code{\link{DecompositionOperator-class}}\cr
	\code{\link{ConstLinDecompOp-class}}\cr
}}
\section{Constructors}{The class is abstract ( \code{contains "VIRTUAL"}).
           It can therefore not be instanciated directly.
           Look at non virtual subclasses and their constructors!

There is also an \href{https://en.wikipedia.org/wiki/Abstract_factory_pattern}{abstract factory} that produces instances of different subclasses depending on the input:

	\code{\link{GeneralDecompOp}}\cr}
