#' Plot the graph of pool connections
#' 
#' @param x The modelrun the connection graph of which is plotted
#' @autocomment 
setMethod(
   f= "plotPoolGraph",
      signature(x="SymbolicModel_by_PoolNames"),
      definition=function 
      (x){
        internalConnections<-lapply(
          x@internal_fluxes, 
          function(flr){
            tuple(
                as.character(flr@sourceName)
                ,as.character(flr@destinationName)
            )
          }
        )
        
        inBoundConnections<-lapply(
            x@in_fluxes,
            function(fl){ as.character(fl@destinationName) }
        )

        outBoundConnections<- lapply(
            x@out_fluxes,
            function(fl){ as.character(fl@sourceName) }
        )
        plotPoolGraphFromTupleLists(internalConnections,inBoundConnections,outBoundConnections)
      
   }
)
#' determine the minimum set of statevariables 
#' 
#' @param object The symbolic model description 
#' @autocomment 
state_variable_names <- function (object){ 
  # we collect all sources and destinations
  # of all fluxes 
  # fixme mm 12/19/2019:
  # we should generalize Fluxes and Fluxrates 
  # by making them subclasses 
  # to classes inbound_connection,outbound_connection,internal_connection and
  # implement the source and destination slots there so that we do not have to
  # discriminate between a rate and a flux as long as we are only interested 
  # in where it is going or coming from
  # the same is true for the drawing of the connection graph
  unique(
    c(
      unlist(
        lapply(
          object@internal_fluxes
          ,function(flr){
            c(
               as.character(flr@sourceName)
              ,as.character(flr@destinationName)
            )
          }
        )
      )
      ,unlist(
        lapply(
          object@in_fluxes,
          function(fl){as.character(fl@destinationName) }
        )
      )
      ,unlist(
        lapply(
          object@out_fluxes,
          function(fl){ as.character(fl@sourceName) }
        )
      )
    )
  )
}
