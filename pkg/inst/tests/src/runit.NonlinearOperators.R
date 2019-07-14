#
# vim:set ff=unix expandtab ts=2 sw=2:
# This test usese the non linear model approach for a linear problem to show that the results are consistent
test.NonlinearOperators=function(){
  require(RUnit)
  t_start=0
  t_end=20
  tn=100
  tol=.02/tn
  #print(tol)
  timestep=(t_end-t_start)/tn
  t=seq(t_start,t_end,timestep)
  k1=1/2
  k2=1/3
  k3=1
  a12=1/9
  a23=1/6
  a21=1/9
  a32=1/6
  Amat <- matrix(
    byrow=T,                                                           
    nrow=3,
    ncol=3,
    c(
      -k1,   a12,    0,  
      a21,   -k2,  a23,  
       0,    a32,  -k3
    )
   )
  A=ConstLinDecompOp(Amat)
  
  alpha=list()
  alpha[["2_to_1"]]=function(C,t){
    a12/k2
  }
  alpha[["3_to_2"]]=function(C,t){
    a23/k3
  }
  alpha[["1_to_2"]]=function(C,t){
    a21/k1
  }
  alpha[["2_to_3"]]=function(C,t){
    a32/k2
  }
  nr=3
  N=matrix( 
     nrow=nr,
     ncol=nr,
     c(
        k1,    0,     0,  
        0  ,  k2,     0,  
        0,     0,    k3 
     )
  )
  f=function(C,t){
    # in this case the application of f can be expressed by a matrix multiplication
    # so we can write f(C,t)  as a Matrix product
    # f(C,t)=N(C,t) C
    # furthermore the matrix N is actually completely linear and even constant
    # so we can further simplify to 
    # f(C,t)=N C
    #  
    return(N%*%C)
  }
  Anl=new("TransportDecompositionOperator",t_start,Inf,nr,alpha,f)
  Anl2=UnBoundNonLinDecompOp(matFunc=function(C,t){Amat})
  
  internal_fluxes=list()
  internal_fluxes[["2_to_1"]]=function(C,t){
    a12*C[2]
  }
  internal_fluxes[["3_to_2"]]=function(C,t){
    a23*C[3]
  }
  internal_fluxes[["1_to_2"]]=function(C,t){
    a21*C[1]
  }
  internal_fluxes[["2_to_3"]]=function(C,t){
    a32*C[2]
  }
  #Anl3=UnBoundNonLinDecompOp(internal_fluxes=internal_fluxes,out_fluxes=out_fluxes)
 
  
  c01=3
  c02=2
  c03=1
  iv=c(sp=c01,fp=c02,mp=c03)

  inputrates=new("TimeMap",t_start,t_end,function(t){return(matrix(
    nrow=nr,
    ncol=1,
    c(
       0,  2,  2
    )
  ))})
  #################################################################################
  # we check if we can reproduce the linear decomposition operator from the
  # nonlinear one
  iv_mat=matrix(nrow=nr,iv)
  Tr=getTransferMatrixFunc(Anl) #this is a function of C and t
  T_00=Tr(iv_mat,0)
  af=getFunctionDefinition(A)
  af_0=af(0)
  checkEquals(af_0,Amat)
  checkEquals(T_00%*%N,Amat)
  
  # We can extract the CompartmentalMatrixFunc directly
  A_0     <-getCompartmentalMatrixFunc(A)(0)
  B_iv_0  <-getCompartmentalMatrixFunc(Anl)(iv_mat,0)
  B2_iv_0 <-getCompartmentalMatrixFunc(Anl2)(iv_mat,0)
  checkEquals(A_0,Amat)
  checkEquals(A_0,B_iv_0)
  checkEquals(A_0,B2_iv_0)
  #        
  ##################################################################################
  ## build the two models (linear and nonlinear)
  #mod=GeneralModel( t, A,iv, inputrates, deSolve.lsoda.wrapper) 
  #modnl=GeneralNlModel( t, Anl, iv, inputrates, deSolve.lsoda.wrapper)
  ## compare the Cstock
  #Y=getC(mod) 
  #Ynonlin=getC(modnl) 
  ##R=getReleaseFlux(mod) 
  ##Rnonlin=getReleaseFlux(modnl) 
#b#egin plots 
  #lt1=2
  #lt2=4
  #m=matrix(c(1,1),1,1,byrow=TRUE)
  #ex=expression(
  #  layout(m),
  #  plot(t,Y[,1],type="l",lty=lt1,col=1,ylab="Concentrations",xlab="Time",ylim=c(min(Y),max(Y))),
  #  lines(t,Ynonlin[,1],type="l",lty=lt2,col=1),
  #  lines(t,Y[,2],type="l",lty=lt1,col=2),
  #  lines(t,Ynonlin[,2],type="l",lty=lt2,col=2),
  #  lines(t,Y[,3],type="l",lty=lt1,col=3),
  #  lines(t,Ynonlin[,3],type="l",lty=lt2,col=3),
  #  legend(
  #  "topright",
  #    c(
  #    "linear sol for pool 1",
  #    "non linear sol for pool 1",
  #    "linear sol for pool 2",
  #    "non linear sol for pool 2",
  #    "linear sol for pool 3",
  #    "non linear sol for pool 3"
  #    ),
  #    lty=c(lt1,lt2),
  #    col=c(1,1,2,2,3,3)
  #  )
  #)
  #plotAndCheck("runit.ThreepSerialFeedback_linear_vs_nonlinear.pdf",ex,environment())
  #checkEquals(
  # Y,
  # Ynonlin,
  # "test non linear solution for C-Content computed by the ode mehtod against analytical",
  # tolerance = tol,
  #)

}
