#
# vim:set ff=unix expandtab ts=2 sw=2:
test.BoundLinDecomOp_init_from_func=function(){
   tstart=0
   tend=0
   f=function(t){2*t}

   obj=BoundLinDecompOp(starttime=tstart,endtime=tend,map=f) #dircet method
}
#----------------------------------------------------------
test.BoundLinDecomOp_init_from_dataframe=function(){
   t=1:20
   inp=seq(1.05,2,0.05)
   tframe=data.frame(times=t,inputrates=inp)
   obj=BoundLinDecompOp(tframe)
   checkEquals(c("t_min"=1,"t_max"=20),getTimeRange(obj))
}
#----------------------------------------------------------
test.BoundLinDecomOp_init_from_dataframe_getTimeRange=function(){
   t=1:20
   inp=seq(1.05,2,0.05)
   tframe=data.frame(times=t,inputrates=inp)
   obj=BoundLinDecompOp(tframe)
   #check if we can get the interpolation function 
   getTimeRange(obj)
}

