
# vim:set ff=unix ts=2 sw=2:
test.allpossible.GeneralDecompOp.arguments<- function(){
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
	decompOps <- lapply(possibleArgs,GeneralDecompOp)
	# We check which subclass is used internally for which argument type 
	print(lapply(decompOps,class))
}
