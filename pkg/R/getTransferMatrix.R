#' deprecated, use getTransferMatrixFunc instead
#' 
#' @param object A compartmental operator
getTransferMatrix<- function(object){
         warning('The function has been renamed to "getTransferMatrixFunc".
                 Please adapt your code to get rid of this warning.')
	     getTransferMatrixFunc(object)
}
