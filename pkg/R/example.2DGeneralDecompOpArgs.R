
# vim:set ff=unix ts=2 sw=2:
example.2DGeneralDecompOpArgs<- function(){
### We present all possibilities to define a 2D \code{\link{DecompOp-class}} 
	possibleArgs <- list(
		# first consider input of non SoilR classes which are a good choice for
	  # standard situations

    # a constant matrix
  	DO_matrix            =matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2)),
		# a matrix valued function
  	DO_matrix_func       =function(t){matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2*(sin(t)+2)))},
		# or time series data provided as lists
		# of either vector and 3Darray
  	DO_list_times_Array	 =example.Time3DArrayList(),
		# or vector an list of matrices
  	DO_list_times_Matrice=example.nestedTime2DMatrixList(),
		
		# internally GeneralDecompOp will convert all its arguments into 
		# classes provided by SoilR
		# You can also do that directly.
		# This is sometimes necessary for non-standard applications or for debugging.
  	DO_TimeMap           =TimeMap(example.Time3DArrayList()),
  	DO_ConstlinDecompOp  =example.ConstlinDecompOpFromMatrix(),
  	DO_BoundLinDecompOp  =example.2DBoundLinDecompOpFromFunction(),
  	DO_UnBoundLinDecompOp=example.2DUnBoundLinDecompOpFromFunction()
	)
	return(possibleArgs)
}
