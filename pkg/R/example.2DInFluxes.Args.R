#' example.2DInFluxes.Args
#' 
#' Create a 2-dimensional examples of a Influx objects from different
#' arguments
#' 
#' 
example.2DInFluxes.Args <- function(){
 		I.vec          =c(5,6) 
		I.vecFunc <-	function(t){
			c(
				1+sin(t),
				2+sin(2*t)
			)
		}
		t.start<- 1
		t.end <- 10
		times <- seq(t.start,t.end,by=1)
		data.list <- lapply(times,I.vecFunc)
		I.list1 <- list(times=times,data=data.list)
			data.array<- array(dim=c(length(I.vec),length(times)))
			for (i in seq_along(times)){
				values <- data.list[[i]]
				data.array[,i] <- unlist(values)
			}
			I.list2 <- list(times=times,data=data.array)
		I.timeMap <- TimeMap(I.list1,interpolation=approxfun)
		I.ConstInFluxes=ConstInFluxes(I.vec)
		I.UnBoundInFluxes=UnBoundInFluxes(I.vecFunc)
		I.BoundInFluxes=BoundInFluxes(I.vecFunc,starttime=t.start,endtime=t.end)
	 return(
		list(
			I.vec						=I.vec,
			I.vecFunc				=I.vecFunc,
			I.list1					=I.list1
			,
		 	I.list2					=I.list2
			,
			I.timeMap				=I.timeMap,
			I.ConstInFluxes		=I.ConstInFluxes,
			I.UnBoundInFluxes	=I.UnBoundInFluxes,
			I.BoundInFluxes		=I.BoundInFluxes
		)
	)
}
