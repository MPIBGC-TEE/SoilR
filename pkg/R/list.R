
# vim:set ff=unix expandtab ts=2 sw=2:
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
	  if (length(map)<2){
	  	stop('Your list has to have at least 2 elements: a vector usually labeled "times" and a list of arrays or matrices.')
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
    if(inherits(data,'list')){
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
    }else{
      if(inherits(data,'array')){
        dd <- dim(data)
        srcDim <-dd[1:(length(dd)-1)] 
		    flatDim=prod(srcDim)
		    arr <- array(dim=c(flatDim,lt),data=as.vector(data))
		    targetClass <-'array'
      }else{
        if(inherits(data,'numeric')){
        srcDim <- 1
		    flatDim=prod(srcDim)
		    arr <- array(dim=c(flatDim,lt),data=data)
		    targetClass <-'numeric'
        }else{
          stop(
            sprintf(
              'The data element of the list must be a list an array or a vector but
               you provided an object of class %s.',
              class(data)))
        }
      }
    }
    funcMaker <- function(i){splinefun(x=times,y=arr[i,])}
		# cut out a time line for every index in the flattene vector
		funcs <- lapply(seq(flatDim),funcMaker)
		arrFunc <- function(t){
			as(
        array(dim=srcDim,data=unlist(lapply(funcs,function(f){f(t)}))),
        targetClass)
		}
		return(TimeMap(map=arrFunc,min(times),max(times)))
  }
)
