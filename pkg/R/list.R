
setMethod(
  f="GeneralDecompOp",
  signature=signature(object="list"),
  #valueClass="BoundLinDecompOp",
  def=function # creates a BoundLinDecompOp from a nested list of a times vector 
  ### an a list of matrices (a matrix for each time step)
  ### The resulting operator is creted by a call to the constructor of class
  ### BoundLinDecompOp
  (object){
    BoundLinDecompOp(TimeMap(object))
  }
)
#-----------------------------------------------------------
setMethod(
  f="TimeMap",
  signature=signature(map="list"),
  #valueClass="BoundLinDecompOp",
  def=function # creates a TimeMap from a nested list of a times vector 
  ### an a list of matrices or vectors (one matrix or vector for each time step)
  (map){
    targetNames <- c('times','data')
    if(identical(intersect(targetNames,names(map)),targetNames)){
	    times <- map[['times']]
	    data  <- map[['data']]
    }
    stop('not implemented yet')
  }
)
