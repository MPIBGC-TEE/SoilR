#
# vim:set ff=unix expandtab ts=2 sw=2:
require(RUnit)
# We present possible scenarios:
# 1.) create an object from valid input
# 2.) try to build an Model object with unsound parameters and 
#     show the savety net in action.
# 3.) force an unsound model to be created that would be rejected by default
# 4.) show some other insensible models being rejected 
#     
#1.) we first create a sensible model
test.correctnessOfModel.impossibleCoefficients=function(){
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
   mess="correctnessOfModel should returnFALSE because the matrix values indicate unbiological behavior (ruwsum should be smaller than zero)"
   checkException(Model(t,A,c(0,0,0),I),mess,silent=TRUE)
}
#2.)
# Now we present some examples where the constructor protests
test.correctnessOfModel.impossibleTimeRanges=function(){
   mess="correctnessOfModel should have returned FALSE, but has not"
   t_start=0 
   t_end=10 
   tdiff=t_end-t_start
   tn=50
   timestep=(tdiff)/tn 
   t=seq(t_start,t_end,timestep) 

   #we create a pseudo A(t) with sensible coeficients 
   #but where the time range begins to late 

   A=BoundLinDecompOp(
      function(times){
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
}
test.correctnessOfModel.correctModel=function(){
   t_start=0 
   t_end=10 
   tn=50
   timestep=(t_end-t_start)/tn 
   t=seq(t_start,t_end,timestep) 
   
   A=BoundLinDecompOp(
     function(times){matrix(nrow=3,ncol=3,byrow=TRUE,
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
  mod=Model(t,A,c(0,0,0),I)
  res<-class(mod)
  checkTrue(res=="Model","correctnessOfModel should have returned TRUE because the model was correct, but has not")
}
