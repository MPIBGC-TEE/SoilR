
# vim:set ff=unix expandtab ts=2 sw=2:


#tupelize <- function(l){
#  # wrap every element into a small list
#  lapply(l,list)}
#

###########################################################################################################
test.listProduct_append_2D <- function(){
	res <- listProduct_append_2D(list(list(c(1,1)),list(c(1,2))),list('a','b'))
  ref <- list(
			list(c(1,1),'a'),
			list(c(1,1),'b'),
			list(c(1,2),'a'),
			list(c(1,2),'b')
	)
	checkEquals(res,ref)
}
  res <- listProduct_append_2D(list(list('a'),list('b')),list(c(1,1),c(1,2)))
  ref <- list(
			list('a',c(1,1)),
			list('a',c(1,2)),
			list('b',c(1,1)),
			list('b',c(1,2))
	)
	checkEquals(res,ref)
###########################################################################################################
test.listProduct_append <- function(){
	res <- listProduct_append(list(list(list(c(1,1)),list(c(1,2))),list('a','b')))
  ref <- list(
			list(c(1,1),'a'),
			list(c(1,1),'b'),
			list(c(1,2),'a'),
			list(c(1,2),'b')
	)
	checkEquals(res,ref)

	res <- listProduct_append(list(list(list(c(1,1)),list(c(1,2))),list('a','b'),list('c')))
  ref <- list(
			list(c(1,1),'a','c'),
			list(c(1,1),'b','c'),
			list(c(1,2),'a','c'),
			list(c(1,2),'b','c')
	)
	checkEquals(res,ref)
	
  res <- listProduct_append(list(list(list(c(1,1))),list('a'),list('c')))
  ref <- list(
			list(c(1,1),'a','c')
	)
	checkEquals(res,ref)
	
  res <- listProduct_append(list(list(list(c(1,1))),list('a'),list('b','c','d')))
  ref <- list(
			list(c(1,1),'a','b'),
			list(c(1,1),'a','c'),
			list(c(1,1),'a','d')
	)
	checkEquals(res,ref)
}
###########################################################################################################
test.tupelize<- function(){
	res <- tupelize(list(c(1,1),c(1,2)))
	ref <- list(list(c(1,1)),list(c(1,2)))
	checkEquals(res,ref)
}
###########################################################################################################
test.tupelize<- function(){
	res <- tupelize(list('a','b'))
	ref <- list(list('a'),list('b'))
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
  
  res <- listProduct(list(c(1,1)),list('a'),list('c'))
  ref <- list(
			list(c(1,1),'a','c')
	)
	checkEquals(res,ref)
	
  res <- listProduct(list(c(1,1)),list('a'),list('c','b'))
  ref <- list(
			list(c(1,1),'a','c'),
			list(c(1,1),'a','b')
	)
	checkEquals(res,ref)
	res <- listProduct(list(c(1,1)),list('a'),list('c'))
  ref <- list(
			list(c(1,1),'a','c')
	)
	checkEquals(res,ref)
	
	
  res <- listProduct(list('a','b'),list(c(1,1),c(1,2)))
  ref <- list(
			list('a',c(1,1)),
			list('a',c(1,2)),
			list('b',c(1,1)),
			list('b',c(1,2))
	)
	checkEquals(res,ref)

  res <- listProduct(list(c(4,4,4),c(4,4,5)),list(c(1,1),c(1,2)))
  ref <- list(
			list(c(4,4,4),c(1,1)),
			list(c(4,4,4),c(1,2)),
			list(c(4,4,5),c(1,1)),
			list(c(4,4,5),c(1,2))
	)
	checkEquals(res,ref)
  res <- listProduct(list(c(1,1)),list('a'))
  ref <- list(
			list(c(1,1),'a')
	)
	checkEquals(res,ref)


}

