#
# vim:set ff=unix expandtab ts=2 sw=2:
#-----------------------------------------------------------
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
#-----------------------------------------------------------
test.TimeMap_from_Vector_and_Vector_scalar_lag <- function(){
   tstart=0
   tend=10
	 lag <- c(1.5,2.5)
   times  <-seq(tstart,tend,by=0.1)
   vec    <-sin(times)+2 
   # such a list can be converted into a TimeMap Object
   obj <- TimeMap(times=times,data=vec,lag=lag)
   tr <- getTimeRange(obj)
   checkEquals(c("t_min"=tstart+max(lag),"t_max"=tend+min(lag)),tr)
   # get the interpolation function and reproduce the data from the list
   Func <- getFunctionDefinition(obj)
   fe <- Func(tstart+max(lag))
   # since lag now is a vector (although the data is still scalar we now expect a vector valued function)
   checkEquals(length(fe),2)
   #data_int<- lapply(times+lag,Func)
   #pp("data_int",environment())
   plot(obj)
   #RUnit::checkIdentical(vec,data_int)
}


#-----------------------------------------------------------
test.TimeMap_from_1Ddataframe_vector_lag <- function(){
   DEACTIVATED('NOT IMPLEMENTED YET')
   tstart=1
   tend=20
   t=tstart:tend
	 lag <- c(1.5,2.5)
   inp=seq(1.05,2,0.05)
   tframe=data.frame(times=t,inputrates=inp)
   obj <- TimeMap(map=tframe,lag=lag)# 1D 
   tr <- getTimeRange(obj)
   checkEquals(c("t_min"=tstart+max(lag),"t_max"=tend+min(lag)),tr)
   matFunc <- getFunctionDefinition(obj)
}
#-----------------------------------------------------------
test.TimeMap_from_ListOfVector_and_Vector_scalar_lag <- function(){
   DEACTIVATED('NOT IMPLEMENTED YET')
   tstart=0
   tend=10
	 lag <- c(1.5,2.5)
   times  <-seq(tstart,tend,by=0.1)
   vec    <-sin(times)+2 
   l <- list(times,vec) 
   # such a list can be converted into a TimeMap Object
   obj=TimeMap(l,lag=lag)
   checkEquals(c("t_min"=min(times)+max(lag),"t_max"=max(times)+min(lag)),getTimeRange(obj))
   # get the interpolation function and reproduce the data from the list
   Func <- getFunctionDefinition(obj)
   data_int<- unlist(lapply(times+lag,Func))
   RUnit::checkIdentical(vec,data_int)
}

#-----------------------------------------------------------
test.TimeMap_from_Vector_and_Array_scalar_lag <- function(){
   DEACTIVATED('NOT IMPLEMENTED YET')
   l <- example.Time3DArrayList()
   times  <- l$times
   arr    <- l$data
   matList <- lapply(seq_along(times),function(i){arr[,,i]})
   
	 lag <- c(1.5,2.5)
   # such a list can be converted into a TimeMap Object
   obj=TimeMap(times=times,data=arr,lag=lag)
   checkEquals(c("t_min"=min(times)+max(lag),"t_max"=max(times)+min(lag)),getTimeRange(obj))
   # get the interpolation function and reproduce the data from the list
   matFunc <- getFunctionDefinition(obj)
   matList_int  <- lapply(l$times+lag,matFunc)
   checkListEqual(matList,matList_int)
}

#-----------------------------------------------------------
test.TimeMap_from_Vector_and_Matrix_scalar_lag <- function(){
   DEACTIVATED('NOT IMPLEMENTED YET')
   l <- example.Time2DArrayList()
   times  <- l$times
   arr    <- l$data
   vecList <- lapply(seq_along(times),function(i){arr[,i]})
	 lag <- c(1.5,2.5)

   # such a list can be converted into a TimeMap Object
   obj <- TimeMap(times=times,data=arr,lag=lag)
   tr <- getTimeRange(obj)
   checkEquals(c("t_min"=min(times)+max(lag),"t_max"=max(times)+min(lag)),tr)
   # get the interpolation function and reproduce the data from the list
   vecFunc <- getFunctionDefinition(obj)
   matList_int  <- lapply(l$times+lag,vecFunc)
   checkListEqual(vecList,matList_int)
}
#-----------------------------------------------------------
test.TimeMap_from_Vector_and_List_scalar_lag <- function(){
   DEACTIVATED('NOT IMPLEMENTED YET')
   l <- example.nestedTime2DMatrixList()
   times   <- l$times
   matList <- l$data
   
   # such a list can be converted into a TimeMap Object
	 lag <- c(1.5,2.5)
   obj=TimeMap(times=times,data=matList,lag=lag)
   tr <- getTimeRange(obj)
   print(tr)
   checkEquals(c("t_min"=min(times)+max(lag),"t_max"=max(times)+min(lag)),tr)
   # get the interpolation function and reproduce the data from the list
   matFunc <- getFunctionDefinition(obj)
   matList_int  <- lapply(l$times+lag,matFunc)
   checkListEqual(matList,matList_int)
}


#-----------------------------------------------------------
test.TimeMap_from_ListOfVector_and_Array_scalar_lag <- function(){
   DEACTIVATED('NOT IMPLEMENTED YET')
   l <- example.Time3DArrayList()
   times  <- l$times
   arr    <- l$data
   matList <- lapply(seq_along(times),function(i){arr[,,i]})
   
   # such a list can be converted into a TimeMap Object
	 lag <- c(1.5,2.5)
   obj=TimeMap(l,lag=lag)
   checkEquals(c("t_min"=min(times)+max(lag),"t_max"=max(times)+min(lag)),getTimeRange(obj))
   # get the interpolation function and reproduce the data from the list
   matFunc <- getFunctionDefinition(obj)
   matList_int  <- lapply(l$times+lag,matFunc)
   checkListEqual(matList,matList_int)
}

#-----------------------------------------------------------
test.TimeMap_from_ListOfVector_and_List_scalar_lag <- function(){
   DEACTIVATED('NOT IMPLEMENTED YET')
   # we use data provided by a SoilR function that
   # is only used in tests and examples
   l <- example.nestedTime2DMatrixList()
   times <- l$times
   matList <-l$data
   
	 lag <- c(1.5,2.5)
   # such a list can be converted into a TimeMap Object
   obj=TimeMap(l,lag=lag)
   tr <- getTimeRange(obj)
   checkEquals(c("t_min"=min(times)+max(lag),"t_max"=max(times)+min(lag)),tr)
   # get the interpolation function and reproduce the data from the list
   matFunc      <- getFunctionDefinition(obj)
   matList_int  <- lapply(l$times+lag,matFunc)
   checkListEqual(matList,matList_int)
}

