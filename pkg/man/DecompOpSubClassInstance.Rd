\name{DecompOpSubClassInstance}
\alias{DecompOpSubClassInstance}
\usage{DecompOpSubClassInstance(object)}
\arguments{
\item{object}{see the method arguments for details}
}
\section{Methods}{	\code{\link{DecompOpSubClassInstance,DecompOp-method}}\cr
	\code{\link{DecompOpSubClassInstance,matrix-method}}\cr
	\code{\link{DecompOpSubClassInstance,TimeMap-method}}\cr}
\title{Generic function to create an object of one of the subclasses of DevompOp}
\description{If the argument is already of a subclass of class DecompOp 
the function returns the unchanged object.
Otherwise it creates an object of a subclass of DecompOp.
Note that the actual class of the output depends on the argument.
For examples, please look at the methods of this function.}
