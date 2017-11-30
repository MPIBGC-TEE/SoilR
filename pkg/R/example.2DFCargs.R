
# vim:set ff=unix ts=2 sw=2:
### Create a 2-dimensionsonal examples  of a Influx objects from different arguments
example.2DFc.Args <- function
# 2D finite time 14C/C  fraction examples
(){
	# the 14C/C fraction of the intput can be given by function
		Fc.vecFunc <-	function(t){
			c(
				1+sin(t),
				2+sin(2*t)
			)
		}
#  	# that is valid in a time interval
#		t.start<- 1
#		t.end <- 10
#
#	# The influx could also be given as timeseries data
#	# We synthesise such data here
#		times <- seq(t.start,t.end,by=1)
#		# as list of vectors
#		data.list <- lapply(times,Fc.vecFunc)
#		# a list combining the timeSeries data can be used as an argument
#		# SoilR will compute the interpolation function and
#		# infer and remember its domain.
#		I.list1 <- list(times=times,data=data.list)
#
#		# the time series could also be given as an array
#		# where the last dimension corresponds to the times
#		# (R will insist our array to be of class 'matrix' which is weierd)
#			data.array<- array(dim=c(length(Fc.vec),length(times)))
#			for (i in seq_along(times)){
#				values <- data.list[[i]]
#				data.array[,i] <- unlist(values)
#			}
#			I.list2 <- list(times=times,data=data.array)
#
#	# If you want to be more specific about the kind of interpolation function SoilR should use,
#	# you can also use SoilRs interpolating facilities directly by creating a TimeMap object
#	# from the time series data. 
#		I.timeMap <- TimeMap(I.list1,interpolation=approxfun)
#
#	# BoundFc will convert the previous arguments to classes internally used by SOilR
#	# but you can also create them directly yourself
    scalar_fraction<- BoundFc(
    	list(
      	times=0:99,
      	data=C14Atm_NH[1:100,2]
     	),
      format="Delta14C"
    )
    saclar_fraction_scalar_delay<- BoundFc(
    	list(
      	times=0:99,
      	data=C14Atm_NH[1:100,2],
				lag=1.1
     	),
      format="Delta14C"
    )
    saclar_fraction_vector_delay<- BoundFc(
    	list(
      	times=0:99,
      	data=C14Atm_NH[1:100,2],
				lag=c(1.1,2.1)
     	),
      format="Delta14C"
    )
#
#
#
#  # We return a list to be used in other examples and tests
	 return(
		list(
			#scalar_fraction=scalar_fraction
			#,
			#saclar_fraction_scalar_delay=saclar_fraction_scalar_delay
			#,
			saclar_fraction_vector_delay=saclar_fraction_vector_delay
		)
	)
}

