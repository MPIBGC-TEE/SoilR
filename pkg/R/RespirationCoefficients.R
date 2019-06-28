#' helper function to compute respiration coefficients
#' 
#' This function computes the respiration coefficients as function of time for
#' all pools according to the given matrix A
#' 
#' 
#' @param A A matrix valued function representing the model.
#' @return A vector valued function of time containing the respiration
#' coefficients for all pools.
RespirationCoefficients=function 
(A 
 ){
   nr=nrow(A(1))
    testvec=matrix(nrow=1,ncol=nr,1)
    rcoeffs= function(t){-testvec%*%A(t)}
    return(rcoeffs)
}
