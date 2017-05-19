#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
require("methods")
require("deSolve")
prefix="../../R/"
globstring=paste(prefix,"*.R",sep="")
auto_paths=Sys.glob(globstring)

for (f in auto_paths){
    source(f,echo=FALSE)
}
#-------------------------------------------------------------------------
t_start=0
t_end=20
tn=100
tol=.02/tn
timestep=(t_end-t_start)/tn
t=seq(t_start,t_end,timestep)
k_S=1/2
k_B=1/3
k_M=1
nr=2

alpha=list()
alpha[["1_to_2"]]=function(C,t){
  0.5
}
#-----------------------------------------
f=function(C,t){
  C_S=C[[1]]
  C_B=C[[2]]
  return(
    matrix(nrow=2,c(
      k_S*C_B*C_S/(k_M+C_S),
      k_B*C_B)
    ))
}
Anl=new("TransportDecompositionOperator",t_start,Inf,nr,alpha,f)
#-----------------------------------------

c01=3
c02=2
iv=c(c01,c02)

#-----------------------------------------
inputrates=BoundInFlux(
  function(t){return(matrix(
    nrow=nr,
    ncol=1,
    c( 2,  0)
  ))} ,
  t_start,
  t_end
)
        
#-----------------------------------------
# build the two models (linear and nonlinear)
modnl=GeneralNlModel( t, Anl, iv, inputrates, deSolve.lsoda.wrapper)
Ynonlin=getC(modnl) 
