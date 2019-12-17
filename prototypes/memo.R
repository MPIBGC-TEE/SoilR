requireNamespace('digest')

fib  <- function(n){
  if(n==0) return(0)
  if( n==1){
    return(1)
  } else {
    return(fib(n-1)+fib(n-2))
  }
}
#fibs <- lapply( 1:32, fib)
#print(fibs)

cache_maker <- function(f){
  cache <- list(0)
  cached_f <- function(n){
    #key <- digest::sha1(n)
    key <- as.character(n)
    if(key %in% names(cache)){
      return(cache[[key]])
    }else{
      res <- f(n)
      l <- list()
      l[[key]] <- res
      cache <<-append(cache,l)
      return(res)
    }
  }
  return(cached_f)
}
cfib <- cache_maker(fib)
fibs <- lapply( 1:30, cfib)
print(fibs)
print("######################################################################")
fibs <- lapply( 1:30, cfib)
print(fibs)


mcfib <- function(cache,n){
  key <- as.character(n)
  if(key %in% names(cache)){
    return(cache[[key]])
  }else{
    if(n==0){
      res <- 0
      cache[['0']] <- 0
      return(c(cache,res))
    }else{ 
      if( n==1){
        res <- 1
        cache[['1']] <- 1
        return(c(cache,res))
      } else {
        res <- mcfib(cache,n-1)+mcfib(cache,n-2)
        cache[[key]] <- res
        return(c(cache,res))

      }
    }
  }
}

# direct fast fac using implicit caching for the recursion
fac <- function(n){
  facs <- as.numeric(c(1))
  for(i in 2:n){facs[[i]] <- facs[[i-1]]*i}
  facs[[n]]
}

# ultra fast fac splitted in two functions 
# 1.) lookup O(1)
look_up_fac <- function(n,cache){
  cache<- caching_facs(n,cache)
  cache[[n]]
}

# 2.) computation of all values up to n (if necessary)
caching_facs <- function(n,cache){
  # we keep the cache if the request can allready be satisfied
  # otherwise we extend it
  l <- length(cache)
  if(n>l){ #cache miss , we have to extend the cache
    for(i in l+1:n){cache[[i]] <- cache[[i-1]]*i} 
  }
  cache 
}
#test it
n_t=6
# direct
print(fac(n_t))
# with caching 
cache <- c(1,2) #initial cache
# train the cache
cache <- caching_facs(n=5,cache)
cache <- caching_facs(n=7,cache)
# lookup
print(look_up_fac(n=n_t,cache))
