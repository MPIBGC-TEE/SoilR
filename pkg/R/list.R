
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
		times <- seq(1,10,by=0.1)
		matFunc <- function(t){matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2*(sin(t)+2)))}
		matList <- lapply(times,matFunc)
		nestedList <-list(times=times,data=matList) 
		map <- nestedList
		if (length(map)<2){
			stop('Your list has to have at least 2 elements.')
		}
    targetNames <- c('times','data')
    if(identical(intersect(targetNames,names(map)),targetNames)){
	    times <- map[['times']]
	    data  <- map[['data']]
    }else{
	   times <- map[[1]]
	   data  <- map[[2]]
    }
		lt <- length(times)
		#remember the shape of the data elements
    fe <- data[[1]]
		srcDim <- dim(fe)
		flatDim=prod(srcDim)
		#remember the class of the data elements
		targetClass <- class(fe)
		# create a 2D  array 
		# with the elements of data flattened to vectors 
		# and  time as second dimension
    
		arr <- array(dim=c(flatDim,lt),data=unlist(lapply(data,as.vector)))
		# cut out a time line for every index in the flattene vector
		funcs <- lapply(1:flatDim,function(i){splinefun(arr[i,],times)})
		arrFunc <- function(t){
			arr(dim=srcDim,data=unlist(lapply(funcs,function(f){f(t)})))
		}
		TimeMap(map=arrFunc,min(times),max(times))
  }
)
