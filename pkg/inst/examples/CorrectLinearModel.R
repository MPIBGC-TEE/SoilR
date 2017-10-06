
# vim:set ff=unix expandtab ts=2 sw=2:
CorrectLinearModel <- function(){
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
}
