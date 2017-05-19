#
# vim:set ff=unix expandtab ts=2 sw=2:
test.TimeMap=function(){
   tstart=0
   tend=0
   f=function(t){2*t} 
   obj=new(Class="TimeMap",tstart,tend,f) #dircet method not encouraged for user code
   obj2=TimeMap.new(tstart,tend,f) #deprecated  initializer 
   t=1:20
   inp=seq(1.05,2,0.05)
   tframe=data.frame(times=t,inputrates=inp)

   obj3=TimeMap(tframe)# a recommended initializer
   checkEquals(c("t_min"=1,"t_max"=20),getTimeRange(obj3))
}

