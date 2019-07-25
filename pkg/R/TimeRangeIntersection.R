
#' The time interval where both functions are defined 
#' 
#' @param obj1 An object on which getTimeRange can be called
#' @param obj2 An object on which getTimeRange can be called
#'
TimeRangeIntersection <- function(obj1,obj2){
    r1=getTimeRange(obj1)
    t1_min=r1["t_min"]
    t1_max=r1["t_max"]

    r2=getTimeRange(obj2)
    t2_min=r2["t_min"]
    t2_max=r2["t_max"]
    
    tr_min<-max(t1_min,t2_min)
    tr_max<-min(t1_max,t2_max)
    if (tr_min>tr_max){
        stop(
            paste("The intervals do not intersect.",obj1,obj2)
        ) 
    }
    c(
       t_min=tr_min
      ,t_max=tr_max
    )    
}
