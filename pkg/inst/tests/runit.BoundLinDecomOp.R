#
# vim:set ff=unix expandtab ts=2 sw=2:
test.BoundLinDecomOp_init_from_func=function(){
   tstart=0
   tend=0
   f=function(t){2*t}

   obj=BoundLinDecompOp(starttime=tstart,endtime=tend,map=f) #dircet method
}
#----------------------------------------------------------
test.BoundLinDecomOp_init_from_matrixfunc=function(){
  # 
# We could also image time series data stored in an array consisting of
	# many stacked matrices, one for each ti`me step.
	# Let us sythesize such a dataset:

	times <- seq(1,10,by=0.1)
	a <- array(dim=c(2,2,length(times)))
	a[1,1,] <- -0.1*(sin(times)+1.1)
	a[2,1,] <-  0
	a[1,2,] <-  0
	a[2,2,] <- -0.2*(sin(times)+1.2)
	# This dataset can also be represented as a matrix valued function of time
	# in the domain [min(times),max(times)]. 
	# BoundLinDecompOp has a method for this signiture.
	dims <- dim(a)
	n <- dims[[1]]
	if (n!=dims[[2]]){
		stop(
			sprintf('The first two dimensions of the array must be equal.	Your array has dimensions: %s.',dims))
	}
	funcs <- list()
	for(i in 1:n){
		for(j in 1:n){
			index=sprintf('%s_%s',i,j)
			funcs[[index]] <- splinefun(times,a[i,j,])
		}
	}
	matFunc <- function(t){
		mat <- matrix(nrow=n,ncol=n)
		for(i in 1:n){
			for(j in 1:n){
				index=sprintf('%s_%s',i,j)
				mat[i,j] <- funcs[[index]](t)
			}
		}
		return(mat)
	}
	A_bl <- BoundLinDecompOp(matFunc,min(times),max(times))
  # Actually SoilR provides some support to create the function and domain from the
  # data automatically and stores the result in an Object of class "TimeMap".
  # BoundLinDecompOp has a method that accepts TimeMap Objects
  # so we do not have to construct the matrix valued function ourselves. 
  tm <- TimeMap(data=a,times=times)
  A_bl_tm <- BoundLinDecompOp(tm)
}
#----------------------------------------------------------
test.BoundLinDecomOp_init_from_dataframe=function(){
   t=1:20
   inp=seq(1.05,2,0.05)
   tframe=data.frame(times=t,inputrates=inp)
   obj=BoundLinDecompOp(tframe)
   checkEquals(c("t_min"=1,"t_max"=20),getTimeRange(obj))
}
#----------------------------------------------------------
test.BoundLinDecomOp_init_from_dataframe_getTimeRange=function(){
   t=1:20
   inp=seq(1.05,2,0.05)
   tframe=data.frame(times=t,inputrates=inp)
   obj=BoundLinDecompOp(tframe)
   #check if we can get the interpolation function 
   getTimeRange(obj)
}

