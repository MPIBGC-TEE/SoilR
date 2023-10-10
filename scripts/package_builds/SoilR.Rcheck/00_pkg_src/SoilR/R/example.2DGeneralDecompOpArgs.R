#' example.2DGeneralDecompOpArgs
#' 
#' We present all possibilities to define a 2D \code{\link{DecompOp-class}}
#' 
#' 
example.2DGeneralDecompOpArgs<- function(){
	possibleArgs <- list(
  	DO_matrix            =matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2)),
  	DO_matrix_func       =function(t){matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2*(sin(t)+2)))},
  	DO_list_times_Array	 =example.Time3DArrayList(),
  	DO_list_times_Matrice=example.nestedTime2DMatrixList(),
  	DO_TimeMap           =TimeMap(example.Time3DArrayList()),
  	DO_ConstlinDecompOp  =example.ConstlinDecompOpFromMatrix(),
  	DO_BoundLinDecompOp  =example.2DBoundLinDecompOpFromFunction(),
  	DO_UnBoundLinDecompOp=example.2DUnBoundLinDecompOpFromFunction()
	)
	return(possibleArgs)
}
