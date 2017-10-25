
# vim:set ff=unix ts=2 sw=2:
test.all_possible_GeneralDecompOp_arguments<- function(){

  DO_matrix             <- GeneralDecompOp(matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2)))
  DO_matrix_func        <- GeneralDecompOp(function(t){matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2*(sin(t)+2)))})
  DO_list_times_Array		<- GeneralDecompOp(example.Time3DArrayList())
  DO_list_times_Matrices<- GeneralDecompOp(example.nestedTime2DMatrixList())
  DO_TimeMap            <- GeneralDecompOp(TimeMap(example.Time3DArrayList()))
  DO_ConstlinDecompOp   <- GeneralDecompOp(example.ConstlinDecompOpFromMatrix())
  DO_BoundLinDecompOp   <- GeneralDecompOp(example.2DBoundLinDecompOpFromFunction())
  DO_UnBoundLinDecompOp <- GeneralDecompOp(example.2DUnBoundLinDecompOpFromFunction())
  
  class(DO_matrix)
  class(DO_matrix_func)
  class(DO_list_times_Array)
  class(DO_list_times_Matrices)
  class(DO_TimeMap)
  class(DO_ConstlinDecompOp)
  class(DO_BoundLinDecompOp)
}
