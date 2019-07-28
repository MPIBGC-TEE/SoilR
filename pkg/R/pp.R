#
# vim:set ff=unix expandtab ts=2 sw=2:

pp=function(# print out an 
	### This function is used to print out a value of a variable together with its name and is helpful for debugging
string,env=parent.frame()){
  cat("\n########################################################################\n")
  callingFun <-as.list(sys.call(-1))[[1]]
  callerName<-toString(callingFun)
  print(paste("pp in",callerName,string,":="))
  print(get(string,env))
}
