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
HIV_R <- function (pars, V_0 = 50000, dV_0 = -200750, T_0 = 100) {
 derivs <- function(time, y, pars) {
 with (as.list(c(pars, y)), {
 dT <- lam - rho * T - bet * T * V
 dI <- bet * T * V - delt * I
 dV <- n * delt * I - c * V - bet * T * V
 return(list(c(dT, dI, dV), logV = log(V)))
 })
 }
 I_0 <- with(as.list(pars), (dV_0 + c * V_0) / (n * delt))
 y <- c(T = T_0, I = I_0, V = V_0)
 out <- ode(y = y, parms = pars, times = times, func = derivs)
 as.data.frame(out)
 }
HIV <- function (pars, V_0 = 50000, dV_0 = -200750, T_0 = 100) {
 I_0 <- with(as.list(pars), (dV_0 + c * V_0) / (n * delt))
 y <- c(T = T_0, I = I_0, V = V_0)
 times <- c(0, 0.1, 0.2, 0.4, 0.6, 0.8, seq(2, 60, by = 2))
 out <- ode(y = y, parms = pars, times = times, func = "derivshiv",
 initfunc = "inithiv", nout = 1, outnames = "logV", dllname = "FME")
 as.data.frame(out)
}
pars <- c(bet = 0.00002, rho = 0.15, delt = 0.55, c = 5.5, lam = 80, n = 900)
out <- HIV(pars = pars)

par(mfrow = c(1, 2))
plot(out$time, out$logV, main = "Viral load", ylab = "log(V)",
 xlab = "time", type = "b")
plot(out$time, out$T, main = "CD4+ T", ylab = "-", xlab = "time", type = "b")
par(mfrow = c(1, 1))

DataLogV <- cbind(time = out$time,
logV = out$logV + rnorm(sd = 0.45, n = length(out$logV)),
sd = 0.45)

ii <- which (out$time %in% seq(0, 56, by = 4))
DataT <- cbind(time = out$time[ii],
T = out$T[ii] + rnorm(sd = 4.5, n = length(ii)),
sd = 4.5)

HIVcost <- function (pars) {
 out <- HIV(pars)
 cost <- modCost(model = out, obs = DataLogV, err = "sd")
#return(cost)
 return(modCost(model = out, obs = DataT, err = "sd", cost = cost))
 }
 HIVcost(pars)$model


