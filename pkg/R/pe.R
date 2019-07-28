pe=function(var,env=parent.frame()){
    ### This function is used to print out a value of an expression together with it and is helpful for debugging
  string=deparse(substitute(var))
  cat("\n########################################################################\n")
  callingFun <-as.list(sys.call(-1))[[1]]
  callerName<-toString(callingFun)
  #res <- eval(string,env)
  res<-var
  cl <-  class(res)
  out <- sprintf(
    'pe in %s:\nExpression\t: %s
    \nResult\t\t: %s 
    \nclass\t\t: %s
    \n'
    , callerName
    ,as.expression(string)
    ,toString(res)
    ,cl
  )
  cat(out)
  cat("\n########################################################################\n")
}
