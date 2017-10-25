#
# vim:set ff=unix expandtab ts=2 sw=2:
test.TimeMapIntern=function(){
   # dircet method not encouraged for user code
   tstart=0
   tend=10
   f=function(t){2*t} 
   obj=new(Class="TimeMap",tstart,tend,f) 
   checkEquals(c("t_min"=tstart,"t_max"=tend),getTimeRange(obj))
}

test.TimeMapDeprecatedConstructor=function(){
   # deprecated  initializer 
   tstart=0
   tend=10
   f=function(t){2*t} 
   obj=TimeMap.new(tstart,tend,f) 
   checkEquals(c("t_min"=tstart,"t_max"=tend),getTimeRange(obj))
}

test.TimeMapFrom1Ddataframe=function(){
   # fixme mm: I would like to deprecate this constructors
   # since a list is better suited 
   t=1:20
   inp=seq(1.05,2,0.05)
   tframe=data.frame(times=t,inputrates=inp)
   obj=TimeMap(tframe)# 1D 
   checkEquals(c("t_min"=1,"t_max"=20),getTimeRange(obj))
}

test.TimeMapFromListOfVectorAndArray=function(){
   obj=TimeMap(example.Time3DArrayList())
   checkEquals(c("t_min"=1,"t_max"=10),getTimeRange(obj))
}

test.TimeMapFromListOfVectorAndList=function(){
   obj=TimeMap(example.nestedTime2DMatrixList())
   checkEquals(c("t_min"=1,"t_max"=10),getTimeRange(obj))
}

