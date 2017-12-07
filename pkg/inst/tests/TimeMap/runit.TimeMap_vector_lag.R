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
test.TimeMap_from_Vector_and_Vector_vector_lag <- function(){
   
  tstart=0
  tend=10
	lag <- c(0.5,5.5)
  times  <-seq(tstart,tend,by=0.1)
  vec    <-sin(times)+2 
  # such a list can be converted into a TimeMap Object
  obj <- TimeMap(times=times,data=vec,lag=lag)
  tr <- getTimeRange(obj)
  checkEquals(c("t_min"=tstart+max(lag),"t_max"=tend+min(lag)),tr)
  # get the interpolation function and reproduce the data from the list
  Func <- getFunctionDefinition(obj)
  fe <- Func(tstart+max(lag))
  # since lag now is a vector (although the data is still scalar) we now expect a vector valued function
  checkEquals(length(fe),2)
  plot(0,0,'n',xlim=c(tstart+min(lag),tend+max(lag)),ylim=c(min(vec),2*max(vec)-min(vec)))
  points(x=times+lag[[1]],vec,pch='.')
  points(x=times+lag[[2]],vec,pch='o')
  add_plot(obj)
  lapply(
    1:length(lag),
    function(i){
      real_times <- times+lag[[i]]
      indices <- which(real_times>=min(tr)&real_times <=max(tr))
      ref <- vec[indices]
      real_times_in_domain <- real_times[indices]
      res <- unlist(
        lapply(
          real_times_in_domain,
          function(t){Func(t)[[i]]}
        )
      )
      RUnit::checkIdentical(ref,res)
    }
    )
}


#-----------------------------------------------------------
test.TimeMap_from_1Ddataframe_vector_lag <- function(){
  #DEACTIVATED('NOT IMPLEMENTED YET')
  tstart=1
  tend=20
  times <- tstart:tend
	lag <- c(1.5,2.5)
  vec=seq(1.05,2,0.05)
  tframe=data.frame(times=times,vec=vec)
  obj <- TimeMap(map=tframe,lag=lag)# 1D 
  tr <- getTimeRange(obj)
  Func <- getFunctionDefinition(obj)
  checkEquals(c("t_min"=tstart+max(lag),"t_max"=tend+min(lag)),tr)
  matFunc <- getFunctionDefinition(obj)
  plot(0,0,'n',xlim=c(tstart+min(lag),tend+max(lag)),ylim=c(min(vec),2*max(vec)-min(vec)))
  points(x=times+lag[[1]],vec,pch='.')
  points(x=times+lag[[2]],vec,pch='o')
  add_plot(obj)
  lapply(
    1:length(lag),
    function(i){
      real_times <- times+lag[[i]]
      indices <- which(real_times>=min(tr)&real_times <=max(tr))
      ref <- vec[indices]
      real_times_in_domain <- real_times[indices]
      res <- unlist(
        lapply(
          real_times_in_domain,
          function(t){Func(t)[[i]]}
        )
      )
      RUnit::checkIdentical(ref,res)
    }
    )
}
#-----------------------------------------------------------
test.TimeMap_from_ListOfVector_and_Vector_vector_lag <- function(){
  #DEACTIVATED('NOT IMPLEMENTED YET')
  tstart=0
  tend=10
	lag <- c(1.5,2.5)
  times  <-seq(tstart,tend,by=0.1)
  vec    <-sin(times)+2 
  l <- list(times,vec) 
  # such a list can be converted into a TimeMap Object
  obj=TimeMap(l,lag=lag)
  tr <- getTimeRange(obj)
  checkEquals(c("t_min"=min(times)+max(lag),"t_max"=max(times)+min(lag)),tr)
  # get the interpolation function and reproduce the data from the list
  Func <- getFunctionDefinition(obj)
  fe <- Func(tstart+max(lag))
  # since lag now is a vector (although the data is still scalar) we now expect a vector valued function
  checkEquals(length(fe),2)
  plot(0,0,'n',xlim=c(tstart+min(lag),tend+max(lag)),ylim=c(min(vec),2*max(vec)-min(vec)))
  points(x=times+lag[[1]],vec,pch='.')
  points(x=times+lag[[2]],vec,pch='o')
  add_plot(obj)
  lapply(
    1:length(lag),
    function(i){
      real_times <- times+lag[[i]]
      indices <- which(real_times>=min(tr)&real_times <=max(tr))
      ref <- vec[indices]
      real_times_in_domain <- real_times[indices]
      res <- unlist(
        lapply(
          real_times_in_domain,
          function(t){Func(t)[[i]]}
        )
      )
      RUnit::checkIdentical(ref,res)
    }
    )
}

#-----------------------------------------------------------
test.TimeMap_from_Vector_and_Array_non_scalar_lags <- function(){
  #DEACTIVATED('NOT IMPLEMENTED YET')
  l <- example.Time3DArrayList()
  times  <- l$times
  arr    <- l$data
  matList <- lapply(seq_along(times),function(i){arr[,,i]})
  # vector lag is not suitable since we have an 2d array per time step 
  checkException(TimeMap(times=times,data=arr,lag=c(1.5,2.5)),'Exception not raised',silent=TRUE)
  # 3D array lag is also not suitable since we have an 2d array per time step 
  checkException(TimeMap(times=times,data=arr,lag=array(dim=c(2,2,2),c(1:8))),'Exception not raised',silent=TRUE)

  # now we build a matrix shape lag that is compatible 
  lag <- matrix(nrow=2,ncol=2,c(1.5,2.5,3.5,4.5))
  obj <- TimeMap(times=times,data=arr,lag=lag)
  tr <- getTimeRange(obj)
  checkEquals(c("t_min"=min(times)+max(lag),"t_max"=max(times)+min(lag)),tr)
 
  # get the interpolation function and reproduce the data from the list
  Func <- getFunctionDefinition(obj)
  #compute the result applied to the start time
  fe <- Func(min(times)+max(lag))
  # since lag now is an array (although the data is still scalar) we now expect an array valued function
  checkEquals(dim(fe),c(2,2))
  plot(0,0,'n',xlim=c(min(times+min(lag)),max(times+max(lag))),ylim=c(min(arr),2*max(arr)-min(arr)))
  points(x=times+lag[[1,1]],arr[1,1,],pch='*')
  points(x=times+lag[[2,1]],arr[2,1,],pch='o')
  points(x=times+lag[[1,2]],arr[1,2,],pch='*')
  points(x=times+lag[[2,2]],arr[2,2,],pch='o')
  add_plot(obj)
  tupels <- list( 
    matrix(nrow=1,c(1,1)),
    matrix(nrow=1,c(1,2)),
    matrix(nrow=1,c(2,1)),
    matrix(nrow=1,c(2,2))
  )
  lapply(
    tupels,
    function(tup){
      real_times <- times+lag[tup]
      indices <- which(real_times>=min(tr)&real_times <=max(tr))
      ref <- unlist(
        lapply(
          indices,
          function(index){
            Tupel <-matrix(nrow=1,c(as.vector(tup),index)) 
            arr[Tupel]
          }
        )               
      )
      real_times_in_domain <- real_times[indices]
      res <- unlist(
        lapply(
          real_times_in_domain,
          function(t){Func(t)[tup]}
        )
      )
      RUnit::checkIdentical(ref,res)
    }
  )
}

#-----------------------------------------------------------
test.TimeMap_from_Vector_and_Matrix_vector_lag <- function(){
   l <- example.Time2DArrayList()
   times  <- l$times
   arr    <- l$data
   vecList <- lapply(seq_along(times),function(i){arr[,i]})
	 lag <- c(1.5,2.5)

   # such a list can be converted into a TimeMap Object
   obj <- TimeMap(times=times,data=arr,lag=lag)
   tr <- getTimeRange(obj)
   plot(obj)
   checkEquals(c("t_min"=min(times)+max(lag),"t_max"=max(times)+min(lag)),tr)
   # get the interpolation function and reproduce the data from the list
   vecFunc <- getFunctionDefinition(obj)
   matList_int  <- lapply(l$times+lag,vecFunc)
   #checkListEqual(vecList,matList_int)
}
#-----------------------------------------------------------
test.TimeMap_from_Vector_and_List_vector_lag <- function(){
   DEACTIVATED('NOT IMPLEMENTED YET')
   l <- example.nestedTime2DMatrixList()
   times   <- l$times
   matList <- l$data
   
	 lag <- c(1.5,2.5)
   obj=TimeMap(times=times,data=matList,lag=lag)
   obj=TimeMap(times=times,data=matList,lag=lag)
   obj=TimeMap(times=times,data=matList,lag=lag)
   tr <- getTimeRange(obj)
   print(tr)
   checkEquals(c("t_min"=min(times)+max(lag),"t_max"=max(times)+min(lag)),tr)
   # get the interpolation function and reproduce the data from the list
   matFunc <- getFunctionDefinition(obj)
}


#-----------------------------------------------------------
test.TimeMap_from_ListOfVector_and_Array_vector_lag <- function(){
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
test.TimeMap_from_ListOfVector_and_List_vector_lag <- function(){
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

