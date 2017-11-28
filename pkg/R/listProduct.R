
# vim:set ff=unix expandtab ts=2 sw=2:
tupelize <- function(l){
  # wrap every element into a small list
  lapply(l,list)}

listProduct_append_2D <- function(lol,l){
  res <- unlist(
      recursive=FALSE,
      lapply(
        lol,
        function(tup){
          lapply(l,function(el){append(tup,list(el))})
        }
	    )
  )
  return(res)
}
listProduct_append <- function(lolol){
  # the first list is the tupel list
  lol <- lolol[[1]]
  # the other lists are the sets for the additional indices
  n <- length(lolol)
  if(n==1){
    res <- lol
  }
  if (n>=2){
    res <- listProduct_append_2D(lol,lolol[[2]])
    if (n> 2){
      # call recursively
      new_lolol <- append(list(res),lolol[3:n])
      res <- listProduct_append(new_lolol)
    }
  }
  return(res)
}

listProduct<- function(...){
  # the function creates a list of tuples( which are implemented as lists)
  # from a list of indexsets (also lists)
  # the resulting tuples are
  l <- list(...)
  if(!all(as.logical(lapply(l,function(sl){inherits(sl,'list')})))){
    stop('The parmeters of the listProduct have to be lists')
  }
  #tupelize the elements of the lists so that we can append to them
  l[[1]] <- tupelize(l[[1]])
  
  return(listProduct_append(l))
}
