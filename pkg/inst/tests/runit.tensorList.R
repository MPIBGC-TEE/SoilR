
# vim:set ff=unix expandtab ts=2 sw=2:

listProduct_2<- function(l1,l2){
   
  f <- function(e1){
		  lapply(
			  l2,
			  function(e2){
				  list(e1,e2)
			  }
      )
  }
	return(
	  unlist(
      recursive=FALSE,
      lapply(l1,f)
    )
	)
}

listProduct<- function(...){
  l <- list(...)
  print("#############################################")
  print('l=')
  print(l)
  if(!all(as.logical(lapply(l,function(sl){inherits(sl,'list')})))){
    stop('The parmeters of the listProduct have to be lists')
  }
  n <- length(l)
  print(sprintf('n=%s',n))
  if(n==1){
    res <- l[[1]]
  }
  if(n==2){
    res <- listProduct_2(l[[1]],l[[2]])
    print('res=')
    print(res)
  }
  if(n >2){
    #res <- unlist(
      #recursive=FALSE,
      res <- listProduct(
             listProduct_2(
               l[[1]],
               l[[2]]
             )
           ,
           #unlist(recursive=FALSE, l[3:n])
           l[3:n]
        )
      #)
  }
  print("#end---------------------------------")
  return(res)
}

##########################################################################################################

test.listProduct_2 <- function(){
	res <- listProduct_2(list(c(1,1),c(1,2)),list('a','b'))
  ref <- list(
			list(c(1,1),'a'),
			list(c(1,1),'b'),
			list(c(1,2),'a'),
			list(c(1,2),'b')
	)
	checkEquals(res,ref)
	
  res <- listProduct_2(list('a','b'),list(c(1,1),c(1,2)))
  ref <- list(
			list('a',c(1,1)),
			list('a',c(1,2)),
			list('b',c(1,1)),
			list('b',c(1,2))
	)
	checkEquals(res,ref)

  res <- listProduct_2(list(c(4,4,4),c(4,4,5)),list(c(1,1),c(1,2)))
  ref <- list(
			list(c(4,4,4),c(1,1)),
			list(c(4,4,4),c(1,2)),
			list(c(4,4,5),c(1,1)),
			list(c(4,4,5),c(1,2))
	)
	checkEquals(res,ref)
}
###########################################################################################################
test.listProduct <- function(){
	res <- listProduct(list(c(1,1),c(1,2)),list('a','b'))
  ref <- list(
			list(c(1,1),'a'),
			list(c(1,1),'b'),
			list(c(1,2),'a'),
			list(c(1,2),'b')
	)
	checkEquals(res,ref)
	
  res <- listProduct(list(c(1,1)),list('a'))
  ref <- list(
			list(c(1,1),'a')
	)
	checkEquals(res,ref)


#	res <- listProduct_2(list(c(1,1),'a'),list('c'))
#  ref <- list(
#			list(c(1,1),'a','c')
#	)
#  print(sprintf('res=%s',paste(res,collapse=',')))
#  print(sprintf('ref=%s',paste(ref,collapse=',')))
#	checkEquals(res,ref)

}
