#
# vim:set ff=unix expandtab ts=2 sw=2:
require(RUnit)
# We present three possible scenarios:
# 1.) create an object from valid input
# 2.) try to build an Model object with unsound parameters and 
#     show the savety net in action.
# 3.) force an unsound model to be created that would be rejected by default
# 4.) show some other insensible models being rejected 
#     
#1.) we first create a sensible model
  t_start=0 
  t_end=10 
  tn=50
  timestep=(t_end-t_start)/tn 
  t=seq(t_start,t_end,timestep) 
  A=BoundLinDecompOp(
    function(times){
      matrix(nrow=3,ncol=3,byrow=TRUE,
    	     c(-1,    0,    0, 
    	      0.5,   -2,    0,   
    	        0,    1, -0.5)
      )    
    },
    t_start,
    t_end
  )  
  I=BoundInFlux(
     function(times){
       matrix(nrow=3,ncol=1,byrow=TRUE,
           c(-1,    0,    0)
       )
     },
     t_start,
     t_end
  )
  res=Model(t,A,c(0,0,0),I)
#2.)
# Now we present some examples where the constructor protests
# test.correctnessOfModel.impossibleCoefficients
   t_start=0 
   t_end=10 
   tn=50
   timestep=(t_end-t_start)/tn 
   t=seq(t_start,t_end,timestep) 

   A=BoundLinDecompOp(
      function(times){
        matrix(nrow=3,ncol=3,byrow=TRUE,
            c(-1,    0,    0, 
            1, -0.7,    0,   
            0,    1, -0.5)
        )
      },
      t_start,
      t_end
   )   
   I=BoundInFlux(
      function(times){
        matrix(nrow=3,ncol=1,byrow=TRUE,
            c(-1,    0,    0)
        )
      },
      t_start,
      t_end
    )
  checkException(
	Model(t,A,c(0,0,0),I), 
	"correctnessOfModel should have returned FALSE 
	because the matrix values indicate unbiological 
	behavior (ruwsum should be smaller than zero), 
	but has not",silent=TRUE)
#3.) 
# force it nevertheless 
	Model(t,A,c(0,0,0),I,pass=TRUE) 

#4.) further examples	
# test.correctnessOfModel.impossibleTimeRanges
   mess="correctnessOfModel should have returned FALSE, but has not"
   t_start=0 
   t_end=10 
   tdiff=t_end-t_start
   tn=50
   timestep=(tdiff)/tn 
   t=seq(t_start,t_end,timestep) 

   #we create an A(t) with sensible coeficients 
   #but where the time range begins to late 

   A=BoundLinDecompOp(
      function(t){
        matrix(nrow=3,ncol=3,byrow=TRUE,
            c(-1,    0,    0, 
            1, -0.7,    0,   
            0,    0.5, -0.5)
        )
      },
      t_start+1/4*tdiff,
      t_end
   )   
   I=BoundInFlux(
      function(times){
        matrix(nrow=3,ncol=1,byrow=TRUE,
            c(-1,    0,    0)
        )
      },
      t_start,
      t_end
    )
   
   checkException(Model(t,A,c(0,0,0),I),mess,silent=TRUE)
   #now we do the same to the InFluxes(t) while A(t) is correct 
   A=BoundLinDecompOp(
      function(times){
        matrix(nrow=3,ncol=3,byrow=TRUE,
            c(-1,    0,    0, 
            1, -0.7,    0,   
            0,  0.5, -0.5)
        )
      },
      t_start,
      t_end
   )   
   I=BoundInFlux(
      function(times){
        matrix(nrow=3,ncol=1,byrow=TRUE,
            c(-1,    0,    0)
        )
      },
      t_start+1/4*tdiff,
      t_end
    )
   checkException(Model(t,A,c(0,0,0),I),mess,silent=TRUE)

   #we create an A(t) with sensible coeficients 
   #but where the time range ends to early 

   A=BoundLinDecompOp(
      function(times){
        matrix(nrow=3,ncol=3,byrow=TRUE,
            c(-1,    0,    0, 
            1, -0.7,    0,   
            0,    0.5, -0.5)
        )
      },
      t_start,
      t_end-1/4*tdiff
   )   
   I=BoundInFlux(
      function(times){
        matrix(nrow=3,ncol=1,byrow=TRUE,
            c(-1,    0,    0)
        )
      },
      t_start,
      t_end
    )
   checkException(Model(t,A,c(0,0,0),I),mess,silent=TRUE)
   #now we do the same to the InFluxes(t) while A(t) is correct 
   A=BoundLinDecompOp(
      function(times){
        matrix(nrow=3,ncol=3,byrow=TRUE,
            c(-1,    0,    0, 
            1, -0.7,    0,   
            0,  0.5, -0.5)
        )
      },
      t_start,
      t_end
   )   
   I=BoundInFlux(
      function(times){
        matrix(nrow=3,ncol=1,byrow=TRUE,
            c(-1,    0,    0)
        )
      },
      t_start,
      t_end-1/4*tdiff
    )
   checkException(Model(t,A,c(0,0,0),I),mess,silent=TRUE)
