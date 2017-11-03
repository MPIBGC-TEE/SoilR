
# vim:set ff=unix ts=2 sw=2:
### Create a 2-dimensionsonal examples  of a Influx objects from different arguments
example.2DInFlux.Args <- function
# 2D finite time linear influx examples
(){
	# if the influx to your model is constant you
	# can represent it by a positive vector with length equal to the number of pools
 		I.vec          =c(5,6) 

	# imagine that your Influx is given by a function
		I.vecFunc <-	function(t){
			c(
				1+sin(t),
				2+sin(2*t)
			)
		}
  	# that is valid in a time interval
		t.start<- 1
		t.end <- 10

	# The influx could also be given as timeseries data
	# We synthesise such data here
		times <- seq(t.start,t.end,by=1)
		# as list of vectors
		data.list <- lapply(times,I.vecFunc)
		# a list combining the timeSeries data can be used as an argument
		# SoilR will compute the interpolation function and
		# infer and remember its domain.
		I.list1 <- list(times=times,data=data.list)

		# the time series could also be given as an array
		# where the last dimension corresponds to the times
		# (R will insist our array to be of class 'matrix' which is weierd)
			data.array<- array(dim=c(length(I.vec),length(times)))
			for (i in seq_along(times)){
				values <- data.list[[i]]
				data.array[,i] <- unlist(values)
			}
			I.list2 <- list(times=times,data=data.array)

	# If you want to be more specific about the kind of interpolation function SoilR should use,
	# you can also use SoilRs interpolating facilities directly by creating a TimeMap object
	# from the time series data. 
		I.timeMap <- TimeMap(I.list1,interpolation=approxfun)

	# GeneralInflux will convert the previous arguments to classes internally used by SOilR
	# but you can also create them directly yourself
		I.ConstInFlux=ConstInFlux(I.vec)
		I.UnBoundInFlux=UnBoundInFlux(I.vecFunc)
		I.BoundInFlux=BoundInFlux(I.vecFunc,starttime=t.start,endtime=t.end)



  # We return a list to be used in other examples and tests
	 return(
		list(
			I.vec						=I.vec,
			I.vecFunc				=I.vecFunc,
			I.list1					=I.list1
			,
		 	I.list2					=I.list2
			,
			I.timeMap				=I.timeMap,
			I.ConstInFlux		=I.ConstInFlux,
			I.UnBoundInFlux	=I.UnBoundInFlux,
			I.BoundInFlux		=I.BoundInFlux
		)
	)
}

