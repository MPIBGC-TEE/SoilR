ydot_maker<-function(
  in_fluxes,
	internal_fluxes,
	out_fluxes,
	time_symbol,
	names
){
  ydot<-function(t,y,parms,...){
    # first evaluate the fluxfuntions at y and t
    num_in_fluxes <- as.numeric(in_fluxes,y,t,time_symbol)
    num_internal_fluxes <- as.numeric(internal_fluxes,y,t,time_symbol)
    num_out_fluxes <- as.numeric(out_fluxes,y,t,time_symbol)
    

    # now compute the derivative of the pool contents from the fluxes
    content_change_rate<- function(name,y,t,time_symbol){
      in_to_name <- ifelse(name %in% names(num_in_fluxes),num_in_fluxes[[name]],0)
      out_from_name <- ifelse(name %in% names(num_out_fluxes),num_out_fluxes[[name]],0)

      internal_to_name <- sum(
        num_internal_fluxes[
          as.character(
            lapply(
              purrr::keep(
                internal_fluxes,
                function(flux){flux@destinationName==name}
              ),
              function(flux){
                src_to_dest_string(
                  flux@sourceName,
                  flux@destinationName
                )
              }
            )
          )
        ]
      )
      internal_from_name <- sum(
        num_internal_fluxes[
          as.character(
            lapply(
              purrr::keep(
                internal_fluxes,
                function(flux){flux@sourceName==name}
              ),
              function(flux){
                src_to_dest_string(
                  flux@sourceName,
                  flux@destinationName
                )
              }
            )
          )
        ]
      )
      in_to_name+internal_to_name-(internal_from_name+out_from_name)
    }
    # ydot is a vector 
    # for every component y_i we have
    # dy_i / d_t =  incomeing_to_i                -         outgoing_from_i
    # dy_i / d_t =  sum(in_to_i)+sum(intern_to_i) -  (sum(internal_from_i)+sum(out_from_i))       

    ydot_num <- as.numeric(
      lapply(
        names,
        function(name) content_change_rate(name,y,t,time_symbol)
      )
    )

    # the fluxes are also the derivative of the accumulated fluxes
    # which we also want to compute
    # So we add them to the derivative and get the accumulated fluxes as part
    # of the solution (This also requires a new startvector extended by as many
    # zeros as there are fluxes
    

    list(
      c(ydot_num,
        num_in_fluxes,
        num_internal_fluxes,
        num_out_fluxes
      ),
      influxes        =num_in_fluxes,
      internal_fluxes =num_internal_fluxes,
      out_fluxes      =num_out_fluxes
    )
  }
  ydot
}
