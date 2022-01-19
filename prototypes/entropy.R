

entropyRatePerJump=function(B,u){
  if(dim(B)[1]==dim(B)[2]) d=dim(B)[1] else(stop("B must be a square matrix"))
  beta=u/sum(u)
  z=-colSums(B)
  lambda=-1*diag(B)
  x=-1*solve(B)%*%u
  y=rbind(x,sum(u))
  Q=rbind(cbind(B,beta),c(z, -1))
  DQ=diag(-1*diag(Q))
  DQy=DQ%*%y
  Pi= DQy/sum(DQy)
  B_lambda=(B+diag(lambda))%*%diag(1/lambda)
  z_lambda=z/lambda
  theta1_1=colSums(-B_lambda*log(B_lambda), na.rm=TRUE)
  theta1=sum(Pi[1:d] * rowSums(cbind(theta1_1, -z_lambda*log(z_lambda)), na.rm = TRUE), na.rm = TRUE)
  theta_z=theta1-(Pi[d+1]*sum(beta*log(beta), na.rm = TRUE))
  theta_P_inf=sum(Pi[1:d]*(1-log(lambda))) + theta_z
  return(theta_P_inf)
}


entropyRatePerTime=function(B,u){
  x=-1*solve(B)%*%u
  ET=sum(x)/sum(u)
  Hp=path_entropy(B,u)
  thetaZ=Hp/ET
  return(thetaZ)
}

pathEntropy=function(B,u){
  if(dim(B)[1]==dim(B)[2]) d=dim(B)[1] else(stop("B must be a square matrix"))
  beta=u/sum(u)
  z=-colSums(B)
  x=-1*solve(B)%*%u
  B0=B; diag(B0)<-0
  H1=sum(beta*log(beta), na.rm = TRUE)
  H2_1=colSums(B0*(1-log(B0)), na.rm = TRUE)
  H2_2=z*(1-log(z))
  H2=sum((x/sum(u))*(rowSums(cbind(H2_1,H2_2),na.rm=TRUE)), na.rm = TRUE)
  H=-H1+H2
  return(H)
}




B2=matrix(c(-1,1,0,-1),2,2); u2=matrix(c(1,0))
B3=matrix(c(-1,0,0,-1),2,2); u3=matrix(c(1,1))
B5=matrix(c(-1,0.5,0.5,-1),2,2); u5=matrix(c(1,1))
B6=matrix(c(-1,1,0,0,-1,1,0,0,-1),3,3); u6=matrix(c(1,0,0))

A1=matrix(c(-0.8,0.104,0,-0.01),2,2); u1=matrix(c(2,4))

entropy_rate_per_jump(B=A1,u=u1)

entropy_rate_per_time(B=B6,u=u6)

path_entropy(B=B3, u=u3)

transitTime(A=A1,u=u1,a=0,q=0.5)
systemAge(A=A1,u=u1,a=0,q=0.5)
