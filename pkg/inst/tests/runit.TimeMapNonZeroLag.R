#
# vim:set ff=unix expandtab ts=2 sw=2:
checkListEqual <- function(matList,matList_int){
   checkEquals(
    sum(
     unlist(
       lapply(
         seq_along(matList),
         function(i){
           difference <- as(matList[[i]]-matList_int[[i]],'matrix')
           norm(difference,type='F') }))), 0)
}
# test the zero lag functionality

test.TimeMapFrom1DdataframeNonZeroLag <- function(){
   # fixme mm: I would like to deprecate this constructors
   # since a list is better suited 
   tstart=1
   tend=20
   t=tstart:tend
	 lag=1.5
   inp=seq(1.05,2,0.05)
   tframe=data.frame(times=t,inputrates=inp)
   obj <- TimeMap(map=tframe,lag=lag)# 1D 
   checkEquals(c("t_min"=tstart+lag,"t_max"=tend+lag),getTimeRange(obj))
   matFunc <- getFunctionDefinition(obj)
}

test.TimeMapFromVectorAndVectorNonZeroLag <- function(){
   tstart=0
   tend=10
	 lag=1.5
   times  <-seq(tstart,tend,by=0.1)
   vec    <-sin(times)+2 
   # such a list can be converted into a TimeMap Object
   obj <- TimeMap(times=times,data=vec,lag=lag)
   tr <- getTimeRange(obj)
   checkEquals(c("t_min"=tstart+lag,"t_max"=tend+lag),tr)
   # get the interpolation function and reproduce the data from the list
   Func <- getFunctionDefinition(obj)
   data_int<- unlist(lapply(times+lag,Func))
   RUnit::checkIdentical(vec,data_int)
}
test.TimeMapFromListOfVectorAndVectorNonZeroLag <- function(){
   tstart=0
   tend=10
	 lag=1.5
   times  <-seq(tstart,tend,by=0.1)
   vec    <-sin(times)+2 
   l <- list(times,vec) 
   # such a list can be converted into a TimeMap Object
   obj=TimeMap(l,lag=lag)
   checkEquals(c("t_min"=min(times)+lag,"t_max"=max(times)+lag),getTimeRange(obj))
   # get the interpolation function and reproduce the data from the list
   Func <- getFunctionDefinition(obj)
   data_int<- unlist(lapply(times+lag,Func))
   RUnit::checkIdentical(vec,data_int)
}

test.TimeMapFromVectorAndArrayNonZeroLag <- function(){
   l <- example.Time3DArrayList()
   times  <- l$times
   arr    <- l$data
   matList <- lapply(seq_along(times),function(i){arr[,,i]})
   
	 lag=1.5
   # such a list can be converted into a TimeMap Object
   obj=TimeMap(times=times,data=arr,lag=lag)
   #checkEquals(c("t_min"=min(times),"t_max"=max(times)),getTimeRange(obj))
   ## get the interpolation function and reproduce the data from the list
   #matFunc <- getFunctionDefinition(obj)
   #matList_int  <- lapply(l$times,matFunc)
   #checkListEqual(matList,matList_int)
}

test.TimeMapFromVectorAndMatrixNonZeroLag <- function(){
   l <- example.Time2DArrayList()
   times  <- l$times
   arr    <- l$data
   vecList <- lapply(seq_along(times),function(i){arr[,i]})
	 lag=1.5

   # such a list can be converted into a TimeMap Object
   obj=TimeMap(times=times,data=arr,lag=lag)
   checkEquals(c("t_min"=min(times)+lag,"t_max"=max(times)+lag),getTimeRange(obj))
   # get the interpolation function and reproduce the data from the list
   vecFunc <- getFunctionDefinition(obj)
   matList_int  <- lapply(l$times+lag,vecFunc)
   checkListEqual(vecList,matList_int)
}

test.TimeMapFromListOfVectorAndArrayNonZeroLag <- function(){
   l <- example.Time3DArrayList()
   times  <- l$times
   arr    <- l$data
   matList <- lapply(seq_along(times),function(i){arr[,,i]})
   
   # such a list can be converted into a TimeMap Object
	 lag=1.5
   obj=TimeMap(l,lag=lag)
   checkEquals(c("t_min"=min(times)+lag,"t_max"=max(times)+lag),getTimeRange(obj))
   # get the interpolation function and reproduce the data from the list
   matFunc <- getFunctionDefinition(obj)
   matList_int  <- lapply(l$times+lag,matFunc)
   checkListEqual(matList,matList_int)
}

test.TimeMapFromListOfVectorAndListNonZeroLag <- function(){
   # we use data provided by a SoilR function that
   # is only used in tests and examples
   l <- example.nestedTime2DMatrixList()
   times <- l$times
   matList <-l$data
   
	 lag=1.5
   # such a list can be converted into a TimeMap Object
   obj=TimeMap(l,lag=lag)
   tr <- getTimeRange(obj)
   print('#############################################')
   print(tr)
   checkEquals(c("t_min"=min(times)+lag,"t_max"=max(times)+lag),getTimeRange(obj))
   # get the interpolation function and reproduce the data from the list
   #matFunc      <- getFunctionDefinition(obj)
   #matList_int  <- lapply(l$times+lag,matFunc)
   #checkListEqual(matList,matList_int)
}

