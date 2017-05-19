# for one dataset
library("FME")
#t_start=0
#t_end=10
#time=seq(t_start,t_end,by=0.1)
##
###time <- -(1:100)/10
#f <- function(time){ 10 * exp(-time / 2) +18*exp(-time/14)}
#C_stock <-f(time) + rnorm(time)/10
##nlmod <- nls(C_stock ~   A * exp(B * time)+C*exp(D*time),start=list(A=9,B=-0.3,C=99,D=-0.1))
##
##plot(time,C_stock, main = "nls(*), data, true function and fit, n=100")
##lines(x=time, y=f(time), col = 4)
##lines(time, predict(nlmod), col = 2)
#Cmod=function(pars){
#        l=as.list(pars)
#	data.frame(t=time,C=l$a*exp(time*l$b) +l$c*exp(time*l$d))
#}
#pars=c(a=10,b=-0.5,c=19,d=-0.2)
#print(pars)
#obs<-data.frame(t=time,C=C_stock,sd=0.4)
#
#C_cost <- function(pars){
#	out1=Cmod(pars)
#	cost1 <- modCost(model=out,obs=obs,err="sd")
#	return(cost)
#}
#C_cost(pars)
Cmod <- function (pars, V_0 = 50000, dV_0 = -200750, T_0 = 100) {
 l=as.list(pars)
 times <- c(0, 0.1, 0.2, 0.4, 0.6, 0.8, seq(2, 60, by = 2))
 return( data.frame(time=times,C=l$a*exp(times*l$b) +l$c*exp(times*l$d)))
}
pars=c(a=1,b=-0.5,c=.5,d=-0.2)
out <- Cmod(pars = pars)


DataC1 <- cbind(time = out$time,
C = out$C + rnorm(sd = 0.45, n = length(out$C)),
sd = 0.45)

DataC2 <- cbind(time = out$time,
C = out$C + rnorm(sd = 0.45, n = length(out$C)),
sd = 0.45)

DataC3 <- cbind(time = out$time,
C = out$C + rnorm(sd = 0.45, n = length(out$C)),
sd = 0.45)

DataC4 <- cbind(time = out$time,
C = out$C + rnorm(sd = 0.45, n = length(out$C)),
sd = 0.45)

DataC5 <- cbind(time = out$time,
C = out$C + rnorm(sd = 0.45, n = length(out$C)),
sd = 0.45)

DataC6 <- cbind(time = out$time,
C = out$C + rnorm(sd = 0.45, n = length(out$C)),
sd = 0.45)


Cmodcost <- function (pars) {
 out <- Cmod(pars)
 cost1 <- modCost(model = out, obs = DataC1, err = "sd")
 cost2 <- modCost(model = out, obs = DataC2, err = "sd",cost=cost1)
 cost3 <- modCost(model = out, obs = DataC3, err = "sd",cost=cost2)
 cost4 <- modCost(model = out, obs = DataC4, err = "sd",cost=cost3)
 cost5 <- modCost(model = out, obs = DataC5, err = "sd",cost=cost4)
 cost6 <- modCost(model = out, obs = DataC6, err = "sd",cost=cost5)
return(cost6)
 }
 Cmodcost(pars)$model
Fit <- modFit(f=Cmodcost,p=pars)
fitted=Cmod(Fit$par)
#print(Fit)
plot(out$time, out$C, main = "C stocks", ylab = "log(V)",
 xlab = "time", type = "b")
lines(fitted)


