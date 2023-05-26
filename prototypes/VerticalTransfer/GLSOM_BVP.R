
GLSOM<-function(xgrid, D, a, k, f, boundary){
  n<-length(xgrid)
#  f1<-function(x){rep(a/D , times=n)}
  f2<-function(x){k(x)/D}
  f3<-function(x){-f(x)/D}
  
  sol<-BVP(f=a/D, g=f2, h=f3, x=range(xgrid), y=boundary, n=length(xgrid)-2)
  return(sol)
}

testModel<-GLSOM(xgrid=d,D=2, a=1.5, k=kf, f=u, boundary = c(1,0))
plot(testModel$U, -d,type="l")
