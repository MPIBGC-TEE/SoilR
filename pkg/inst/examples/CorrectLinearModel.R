
# vim:set ff=unix set expandtab set ts=2 sw=2:
CorrectLinearModel <- function(){
  ## We first specify the points in time where we want to compute results
  t_start=0 
  t_end=10 
  tn=50
  timestep <- (t_end-t_start)/tn 
  times <- seq(t_start,t_end,timestep) 
  A=BoundLinDecompOp(
    ## The first argument is a matrix-valued function of time
    function(t){
      matrix(nrow=3,ncol=3,byrow=TRUE,
         c(
           -1,    0,        0,
          0.5,   -2,        0,
            0,    1, sin(t)-1 
        )
      )    
    },
    ## The other two arguments describe the time interval where the function is valid (the domain of the function)
    ## This interval must include all times specified in the \code{times} argument of the model. You can also use 
    starttime=0,
    endtime=20
  )  
  I=BoundInFlux(
     ## The first argument is a vector-valued function of time
     function(t){
       matrix(nrow=3,ncol=1,byrow=TRUE,
           c(-1,    0,    0)
       )
     },
     ## The other two arguments describe the time interval where the function is valid (the domain of the function)
     t_start,
     t_end
  )
  res=Model(times,A,c(0,0,0),I)
}
