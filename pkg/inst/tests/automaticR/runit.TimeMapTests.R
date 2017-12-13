# This file has been automatically created by the script "exampleMaker.R". Change this script for permanent changes.
test.TimeMap.bfc_1 <- function(){
# A function that is valid in a time interval bounded by tstart and tend 
  

 bfc_1 <- TimeMap(
	map = function(t){
				c(
					1+sin(t),
					2+sin(2*t)
				)
			},
	starttime = 1,
	endtime = 10
)
plot(bfc_1)
}
test.TimeMap.bfc_2 <- function(){
# a vector of times, a vector of scalar fractions per time step and a vector lag 
  

 bfc_2 <- TimeMap(
	times = 0:99,
	data = C14Atm_NH[1:100,2],
	lag = c(1.1,1.2)
)
plot(bfc_2)
}
test.TimeMap.bfc_3 <- function(){
# a vector of times,a vector of scalar fractions per time step and a scalar lag 
  

 bfc_3 <- TimeMap(
	times = 0:99,
	data = C14Atm_NH[1:100,2],
	lag = 1.1
)
plot(bfc_3)
}
