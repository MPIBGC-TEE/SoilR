setClass(
    Class="BoundLinDecompOp",
    contains=c("DecompOp","TimeMap"),   
   )
correctnessOfBoundDecompOp <- function(obj){
  tr <- getTimeRange(obj)
  t_min <- tr[['t_min']]
  matFunc <- getFunctionDefinition(obj)
  testVal <- matFunc(t_min)
  valDims <- dim(testVal)
  if (length(valDims)!=2){
    stop(sprintf('The function must return a 2-dimensional object (like a Matrix). Your input leads to a function that return an object with dim(object)=%s',valDims))}
  if (valDims[[1]]!=valDims[[2]]){
    stop(sprintf('The function must return a quadratic object (qudratic Matrix). Your input leads to a function that return an object with dim(object)=%s',valDims))}
}
setMethod(
      f="BoundLinDecompOp",
      signature=c(map="UnBoundLinDecompOp"),
      definition=function 
      (map,
       starttime=-Inf, 
       endtime=Inf   
       ){
      f=getFunctionDefinition(map)
      return(BoundLinDecompOp(map=f,starttime,endtime))
     }
)
setMethod(
      f="BoundLinDecompOp",
      signature=signature(map="ANY"),
      definition=function 
  (
    map, 
    ...  
  ){
    tm <- TimeMap(map,...)
    obj <- as(tm,"BoundLinDecompOp")
    return(obj)
  }
)
