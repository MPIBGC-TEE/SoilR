# A function that is valid in a time interval bounded by tstart and tend 
  

 tm_1 <- TimeMap(
	map = function(t){
				c(
					1+sin(t),
					2+sin(2*t)
				)
			},
	starttime = 1,
	endtime = 10
)
# a vector of times, a vector of scalar fractions per time step and a vector lag 
  

 tm_2 <- TimeMap(
	times = 0:99,
	data = C14Atm_NH[1:100,2],
	lag = c(1.1,1.2)
)
# We could also imagine time series data
# stored in an array consisting of
# many stacked vectors, one for each time step.
# and combine both to a list. 
 
times <- seq(1,10,by=0.1)
a <- array(dim=c(2,length(times)))
a[1,] <- -0.1*(sin(times)+1.1)
a[2,] <- -0.2*(sin(times)+1.2) 

 tm_3 <- TimeMap(
	map = list(times=times,data=a)
)
