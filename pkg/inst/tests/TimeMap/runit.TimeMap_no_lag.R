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
test.TimeMapInternZeroLag <- function(){
   # dircet method not encouraged for user code
   tstart=0
   tend=10
   f=function(t){2*t} 
   obj=new(Class="TimeMap",tstart,tend,f) 
   checkEquals(c("t_min"=tstart,"t_max"=tend),getTimeRange(obj))
}

test.TimeMapDeprecatedConstructorZeroLag <- function(){
   # deprecated  initializer 
   tstart=0
   tend=10
   f=function(t){2*t} 
   obj=TimeMap.new(tstart,tend,f) 
   checkEquals(c("t_min"=tstart,"t_max"=tend),getTimeRange(obj))
}

test.TimeMapFrom1DdataframeZeroLag <- function(){
   # fixme mm: I would like to deprecate this constructors
   # since a list is better suited 
   t=1:20
   inp=seq(1.05,2,0.05)
   tframe=data.frame(times=t,inputrates=inp)
   obj <- TimeMap(tframe)# 1D 
   checkEquals(c("t_min"=1,"t_max"=20),getTimeRange(obj))
   matFunc <- getFunctionDefinition(obj)
}

test.TimeMapFromVectorAndVectorZeroLag <- function(){
   tstart=0
   tend=10
   times  <-seq(tstart,tend,by=0.1)
   vec    <-sin(times)+2 
   # such a list can be converted into a TimeMap Object
   obj=TimeMap(times=times,data=vec)
   checkEquals(c("t_min"=min(times),"t_max"=max(times)),getTimeRange(obj))
   # get the interpolation function and reproduce the data from the list
   Func <- getFunctionDefinition(obj)
   data_int<- unlist(lapply(times,Func))
   print(data_int)
   print(vec)
   RUnit::checkIdentical(vec,data_int)
}
test.TimeMapFromListOfVectorAndVectorZeroLag <- function(){
   tstart=0
   tend=10
   times  <-seq(tstart,tend,by=0.1)
   vec    <-sin(times)+2 
   l <- list(times,vec) 
   # such a list can be converted into a TimeMap Object
   obj=TimeMap(l)
   checkEquals(c("t_min"=min(times),"t_max"=max(times)),getTimeRange(obj))
   # get the interpolation function and reproduce the data from the list
   Func <- getFunctionDefinition(obj)
   data_int<- unlist(lapply(times,Func))
   RUnit::checkIdentical(vec,data_int)
}

test.TimeMapFromVectorAndArrayZeroLag <- function(){
   l <- example.Time3DArrayList()
   times  <- l$times
   arr    <- l$data
   matList <- lapply(seq_along(times),function(i){arr[,,i]})
   
   # such a list can be converted into a TimeMap Object
   obj=TimeMap(times=times,data=arr)
   checkEquals(c("t_min"=min(times),"t_max"=max(times)),getTimeRange(obj))
   # get the interpolation function and reproduce the data from the list
   matFunc <- getFunctionDefinition(obj)
   matList_int  <- lapply(l$times,matFunc)
   checkListEqual(matList,matList_int)
}

test.TimeMapFromVectorAndMatrixZeroLag <- function(){
   l <- example.Time2DArrayList()
   times  <- l$times
   arr    <- l$data
   vecList <- lapply(seq_along(times),function(i){arr[,i]})
   
   # such a list can be converted into a TimeMap Object
   obj=TimeMap(times=times,data=arr)
   checkEquals(c("t_min"=min(times),"t_max"=max(times)),getTimeRange(obj))
   # get the interpolation function and reproduce the data from the list
   vecFunc <- getFunctionDefinition(obj)
   matList_int  <- lapply(l$times,vecFunc)
   checkListEqual(vecList,matList_int)
}

test.TimeMapFromListOfVectorAndArrayZeroLag <- function(){
   l <- example.Time3DArrayList()
   times  <- l$times
   arr    <- l$data
   matList <- lapply(seq_along(times),function(i){arr[,,i]})
   
   # such a list can be converted into a TimeMap Object
   obj=TimeMap(l)
   checkEquals(c("t_min"=min(times),"t_max"=max(times)),getTimeRange(obj))
   # get the interpolation function and reproduce the data from the list
   matFunc <- getFunctionDefinition(obj)
   matList_int  <- lapply(l$times,matFunc)
   checkListEqual(matList,matList_int)
}

test.TimeMapFromListOfVectorAndListZeroLag <- function(){
   # we use data provided by a SoilR function that
   # is only used in tests and examples
   l <- example.nestedTime2DMatrixList()
   times <- l$times
   matList <-l$data
   
   # such a list can be converted into a TimeMap Object
   obj=TimeMap(l)
   checkEquals(c("t_min"=min(times),"t_max"=max(times)),getTimeRange(obj))
   # get the interpolation function and reproduce the data from the list
   matFunc      <- getFunctionDefinition(obj)
   matList_int  <- lapply(l$times,matFunc)
   checkListEqual(matList,matList_int)
}

