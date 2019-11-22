requireNamespace('deSolve')
times<-1:100
startValues=c(barrel=2,bottle=2) #vector
ks<-c(k_barrel=0.1,k_bottle=0.2)
ydot<-function(t,y,parms,...){
  with(as.list(c(parms,y)),{
    ydot<- -c(
      k_barrel*barrel,
      k_bottle*bottle
    )
    outfluxes<-c(barrel=k_barrel*barrel,bottle=k_bottle*bottle)
    list(ydot,outfluxes=outfluxes)
  })
}
print(ydot(1,startValues,ks))
deSolve::lsoda(y=startValues,times=times,func=ydot,parms=ks)

