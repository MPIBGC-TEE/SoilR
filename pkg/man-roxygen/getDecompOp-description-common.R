#' @return The actual class of the result can vary. 
#' It will be a subclass of \code{\linkS4class{DecompOp}}. 
#' These objects are an abstraction for a complete description of the fluxes in
#' the pool system regardless of the form it is provided in.
#' The information contained in these objects is equivalent to the set of internal and outward fluxes as
#' functions of pool contents and time and sufficient to infer
#' the "Compartmental Matrix" as a matrix valued function of the same arguments.
#' In the general case of a nonautonomous nonlinear Model this function
#' is a true function of  both, the pool contents and time.
#' In the case of an non-autonomous linear model it is a function of time only,
#' and in case of a autonomous linear model it is a constant matrix.
#' The vector valued function can be inferred by the generic  
#' function \code{\link{getFunctionDefinition}}. 
