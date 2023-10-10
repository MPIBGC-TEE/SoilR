apply_to_state_vec_and_time <- function(func,y,t,time_symbol){
    all_args <- c(y,t)
    names(all_args) <- c(names(y),time_symbol)
    arg_names <- names(formals(func))
    arg_values<- as.list(all_args[arg_names])
    do.call(func,arg_values)
}
