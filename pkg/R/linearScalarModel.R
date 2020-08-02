#' Implementation of a general model for linear non-autonomous systems with scalar modifiers
#' 
#' This function implements a linear model with scalar modifier for inputs
#' and compartmental matrix.
#' 
#' 
#' @param t A vector containing the points in time where the solution is
#' sought.
#' @param A A square (n x n) matrix with compartmental structure
#' @param C0 A vector of length n containing the initial amount of carbon for
#' the n pools.
#' @param u A vector of length n with constant mass inputs for the n pools.
#' @param gamma A scalar or data.frame object specifying the modifier for the
#' mass inputs.
#' @param xi A scalar, data.frame, function or anything that can be 
#' converted to a scalar function of time \code{\linkS4class{ScalarTimeMap}} 
#' object  specifying the external  (environmental and/or edaphic) effects on
#' decomposition rates.
#' @param xi_lag A time shift/delay  for the automatically 
#' created time dependent function xi(t) 
#' @param solver A function that solves the system of ODEs. This can be
#' \code{\link{euler}} or \code{\link{deSolve.lsoda.wrapper}} or any other user
#' provided function with the same interface.
#' @param pass Conditional. Whether a check for compartmental matrix is applied.
#' @return A Model Object that can be further queried
#' @seealso \code{\link{RothCModel}}. There are other
#' \code{\link{predefinedModels}} and also more general functions like
#' \code{\link{Model}}.
#' @references C.A., M. Mueller, S.E. Trumbore. 2012. Models of soil organic
#' matter decomposition: the SoilR package version 1.0. Geoscientific Model
#' Development 5, 1045-1060.
#' @examples
#' t=seq(0,52*200,1) # Fix me! Add an example.  
linearScalarModel<- function 
  (t,      
   A,  
   C0,	
   u,    
   gamma, 
   xi,  
   xi_lag=0,
   solver=deSolve.lsoda.wrapper,
   pass=FALSE  
  )	
  { 
    t_start=min(t)
    t_end=max(t)
    
    if(ncol(A)!=nrow(A)) stop("The matrix A must be a square matrix")
    if(ncol(A)!=length(u)) stop("Dimension of A must match length of u")

    if(length(gamma)==1){
      inputFluxes=BoundInFluxes(
        function(t){gamma*u},
        t_start,
        t_end
      )
    }
    if(class(gamma)=="data.frame"){
      x=gamma[,1]  
      y=gamma[,2]  
      inputScalar=splinefun(x,y)
      inputFluxes=BoundInFluxes(
        function(t){inputScalar(t)*u},
        min(x),
        max(x)
      )
    }
    # whatever format xi is given in we convert it to a time map object
    # (function,constant,data.frame,list considering also the xi_lag argument)
    xi=ScalarTimeMap(xi,lag=xi_lag)
    fX=getFunctionDefinition(xi)
    At=ConstLinDecompOpWithLinearScalarFactor(mat=A,xi=xi)
    Mod=GeneralModel(t=t,A=At,ivList=C0,inputFluxes=inputFluxes,solverfunc=solver,pass=pass)
    return(Mod)
  }
