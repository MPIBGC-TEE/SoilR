#helper function to build condition objects (S3 class)
condition <- function(subclass, message, call = sys.call(-1), ...) {
  # from http://adv-r.had.co.nz/beyond-exception-handling.html
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}
# function to create a special kind of error (a constructor)
create_trouble1_error <- function(arg){
    condition(
      subclass=c('trouble1_error',"error")
      ,message='trouble of the first kind'
      ,call=sys.call(-1)
      ,arg=arg # add some information for what argument might have coused the trouble
    )
}
# another function to create another special kind of error (constructor for another subclass)
create_trouble2_error <- function(){
    condition(
      subclass=c('trouble2_error',"error")
      ,message='trouble of kind 2'
      ,call=sys.call(-1)
    )
}



# a function that can possibly fail
r_check<- function(arg,trouble1=FALSE,trouble2=FALSE){
  # trouble1 and trouble2 are just there for mocking two kinds of behavior 
  # the reaal function would have only the arg argument
  if(trouble1){
    stop(create_trouble1_error(arg)) 
  }
  if(trouble2){
    # create another condition  that is also subclass of error
    stop(create_trouble2_error())
  }

  res <- "fine"
  res
}

evalCalls <- function(calls,func){
  # this one will obviously fail and print nothing 
  # except the error message 
  # if func does not catch the erros
  unlist( lapply( calls,func))
}

########################################################################
#without error handling
f1 <- function(cll){ eval(cll) }

########################################################################
#with tryCatch
f2 <- function(cll){
  # this anonymous function does handle the errors immediately
  res <- tryCatch(
    expr=eval(cll)
    # install something to deal with trouble1_errros
    ,trouble1_error=function(e){toString(e)}
    # install something to deal with trouble2_errros
    ,trouble2_error=function(e){c(toString(e))}
  )
  res # this will be either the result or the return value 
  # of the error handling functions
}

########################################################################
f1_with_restarts <- function(cll){
  # this function does not handle the errors 
  # if called directly it will not recover
  # but it provides some recovery_strategies each of which can be called
  # from higher up giving the caller that handles the exception the
  # choice of how to recover.
  res <- withRestarts(
    eval(cll)
    ,report=function(e){
        paste('An error of kind',toString(e),'occured',collapse=' ')
    }
    ,apologize=function(e){ 'I am so sorry.'}
  )
}
########################################################################
eval_and_apologize<- function(calls,func){
  # this one will use withCallingHandlers that call the apologizing restarts of func
  withCallingHandlers(
     # The errors are handeled outside (and could be handled much higher up in the call stack)
     # if we call the function from somewhere else we could use other restarts
     trouble1_error=function(e) {invokeRestart("apologize",e=e)}
    ,trouble2_error=function(e) {invokeRestart("apologize",e=e)}
    ,unlist(lapply(calls,func))
  )
}
########################################################################
eval_and_report<- function(calls,func){
  # this one will use withCallingHandlers that call the reporting restarts of func
  withCallingHandlers(
     # The errors are handeled outside (and could be handled much higher up in the call stack)
     # if we call the function from somewhere else we could use other restarts
     trouble1_error=function(e) {invokeRestart("report",e=e)}
    ,trouble2_error=function(e) {invokeRestart("report",e=e)}
    ,unlist(lapply(calls,func))
  )
}
########################################################################
# print nicely
wrap <- function(name,lines){
  cat(
    paste(
      paste("############################## ",name,"###############",collapse="")
      ,'\n'
      ,paste(lines,collapse='\n')
      ,'\n'
      ,collapse=""
    )
  )
}

########################################################################
#main
p="pkgName"
calls <- c(
  as.call(list(r_check,arg=p,trouble1=TRUE))
  ,as.call(list(r_check,arg=p,trouble2=TRUE))
  ,as.call(list(r_check,arg=p))
)
#print(evalCalls(calls,f1)) will fail since no error handling
wrap("tryCatch",evalCalls(calls,f2))
wrap("apologize restart",eval_and_apologize(calls,f1_with_restarts))
wrap("report restart",eval_and_report(calls,f1_with_restarts))
