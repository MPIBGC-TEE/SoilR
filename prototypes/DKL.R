
# Kullback-Leibler distance between age and transit time distributions
# this is an experimental test for comparing age and transit time distributions

library(SoilR)
library(expm)

# The Harvard Forest Model
ks = c(kr = 1/1.5, koi = 1/1.5, koeal = 1/4, koeah = 1/80,
       kA1 = 1/3, kA2 = 1/75, kM = 1/110)
A = -abs(diag(ks))
A[3, 2] = ks[2] * (98/(3 + 98 + 51))
A[4, 3] = ks[3] * (4/(94 + 4))
A[6, 5] = ks[5] * (24/(6 + 24))
A[7, 6] = ks[6] * (3/(22 + 3))
A[7, 2] = ks[2] * (3/(3 + 98 + 51))
A[4, 1] = ks[1] * (35/(35 + 190 + 30))
A[5, 1] = ks[1] * (30/(35 + 190 + 30))

LI = 150 #Litter inputs
RI = 255 #Root inputs
In=matrix(nrow = 7, ncol = 1, c(RI, LI, 0, 0, 0, 0, 0))

ages=seq(0,200) # Time vector for density functions
SA=systemAge(A=A, u=In, a=ages)
TT=transitTime(A=A, u=In, a=ages)

pathEntropy(A=A, u=In)

plot(ages, TT$transitTimeDensity*log(TT$transitTimeDensity/SA$systemAgeDensity), type="l")
abline(h=0, lty=2)

DKL<-function(A, u){
  beta = u/sum(u)
  z = -colSums(A)
  x = -1 * solve(A) %*% u
  eta = x/sum(x)
  integrand<-function(tau){
    fT<- z %*% expm(tau*A) %*% beta
    fA<- z %*% expm(tau*A) %*% eta
    as.numeric(fT * log(fT/fA))
  }
  integrate(f=Vectorize(integrand), lower=0, upper=Inf)$value
}

DKL(A=A, u=In)

A1<-diag(-0.1, nrow=7)

DKL(A=A1, u=In)

