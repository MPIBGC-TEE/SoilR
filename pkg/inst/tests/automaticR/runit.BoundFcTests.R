# This file has been automatically created by the script "exampleMaker.R". Change this script for permanent changes.
test.BoundFc.bfc_1 <- function(){
# A function that is valid in a time interval bounded by tstart and tend 
  

 bfc_1 <- BoundFc(
	map = function(t){
				c(
					1+sin(t),
					2+sin(2*t)
				)
			},
	starttime = 1,
	endtime = 10,
	format = 'Delta14C'
)
plot(bfc_1)
}
test.BoundFc.bfc_2 <- function(){
# A function that is valid in a time interval bounded by tstart and tend 
  

 bfc_2 <- BoundFc(
	map = function(t){
				c(
					1+sin(t),
					2+sin(2*t)
				)
			},
	starttime = 1,
	endtime = 10,
	format = 'AbsoluteFractionModern'
)
plot(bfc_2)
}
test.BoundFc.bfc_3 <- function(){
# a vector of times, a vector of scalar fractions per time step and a vector lag 
  

 bfc_3 <- BoundFc(
	times = 0:99,
	data = C14Atm_NH[1:100,2],
	lag = c(1.1,1.2),
	format = 'Delta14C'
)
plot(bfc_3)
}
test.BoundFc.bfc_4 <- function(){
# a vector of times, a vector of scalar fractions per time step and a vector lag 
  

 bfc_4 <- BoundFc(
	times = 0:99,
	data = C14Atm_NH[1:100,2],
	lag = c(1.1,1.2),
	format = 'AbsoluteFractionModern'
)
plot(bfc_4)
}
test.BoundFc.bfc_5 <- function(){
# We could also imagine time series data
# stored in an array consisting of
# many stacked vectors, one for each time step.
# and combine both to a list. 
 
times <- seq(1,10,by=0.1)
a <- array(dim=c(2,length(times)))
a[1,] <- -0.1*(sin(times)+1.1)
a[2,] <- -0.2*(sin(times)+1.2) 

 bfc_5 <- BoundFc(
	map = list(times=times,data=a)
)
plot(bfc_5)
}
test.BoundFc.bfc_6 <- function(){
# We could also imagine time series data
# stored in an array consisting of
# many stacked vectors, one for each time step.
# and combine both to a list. 
 
times <- seq(1,10,by=0.1)
a <- array(dim=c(2,length(times)))
a[1,] <- -0.1*(sin(times)+1.1)
a[2,] <- -0.2*(sin(times)+1.2) 

 bfc_6 <- BoundFc(
	map = list(times=times,data=a)
)
plot(bfc_6)
}
test.BoundFc.bfc_7 <- function(){
# a vector of times,a vector of scalar fractions per time step and a scalar lag 
  

 bfc_7 <- BoundFc(
	times = 0:99,
	data = C14Atm_NH[1:100,2],
	lag = 1.1,
	format = 'Delta14C'
)
plot(bfc_7)
}
test.BoundFc.bfc_8 <- function(){
# a vector of times,a vector of scalar fractions per time step and a scalar lag 
  

 bfc_8 <- BoundFc(
	times = 0:99,
	data = C14Atm_NH[1:100,2],
	lag = 1.1,
	format = 'AbsoluteFractionModern'
)
plot(bfc_8)
}
