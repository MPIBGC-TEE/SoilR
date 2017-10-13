
# vim: ff=unix expandtab ts=2 sw=2:
CorrectNonautonomousLinearModelExplicit <- function(){
  # This example describes the creation and use of a Model object that 
  # is defined by time dependent functions for decomposition and influx.
  # The constructor of the Model-class  (see  ?Model) is a 'Generic' 
  # function  that  has 'Methods' for different combinations of 
  # arguments.
  # In this example we will call the constructor whith arguments which 
  # are of the same type as the current internal representations in the 
  # Model object. and create these arguments explicitly beforehand. 
  # See other examples for the generic constructor Model 
  # We start with the Decomposition Operator.
  # For this example we assume that we are able to describe it 
  # by explicit R functions and therefore choose the appropriate
  # sub class BoundLinDecompOp
  # of DecompOp explicitly.  (see ?'BoundLinDecompOp-class') 
  A=BoundLinDecompOp(
    ## We call the generic constructor (see ?BoundLindDcompOp) 
    ## which has a method  
    ## that takes a matrix-valued function of time as its first argument.
    function(t){
      matrix(nrow=3,ncol=3,byrow=TRUE,
         c(
           -1,    0,        0,
          0.5,   -2,        0,
            0,    1, sin(t)-1 
        )
      )    
    },
    ## The other two arguments describe the time interval where the 
    ## function is valid (the domain of the function)
    ## The interval will be checked against the domain of the InFlux
    ## argument of Model and against its 't' argument to avoid 
    ## invalid computations outside the domain. 
    ##  Inf and -Inf are possible values. 
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
     ## The other two arguments describe the time interval where the 
     ## function is valid (the domain of the function)
     starttime=0,
     endtime=40
  )
  ## No we specify the points in time where we want 
  ## to compute results
  t_start=0 
  t_end=10 
  tn=50
  timestep <- (t_end-t_start)/tn 
  times <- seq(t_start,t_end,timestep) 
  ## and the start values
  sv=c(0,0,0)
  mod=Model(t=times,A,sv,I)

  ## No we use the model to compute some results
  getC(mod)
  getReleaseFlux(mod)
  #also look at the methods section of Model-class 
}
