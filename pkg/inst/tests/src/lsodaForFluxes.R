#!/usr/bin/Rscript
requireNamespace('deSolve')
requireNamespace('RUnit')
requireNamespace("pkgload")
requireNamespace("purrr")
pkgload::load_all('../../../')


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
      out_from_name <- ifelse(name %in% names(num_out_fluxes),num_in_fluxes[[name]],0)

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
#-------------------------------------------------
IVP_maker <- function(
  in_fluxes,
	internal_fluxes,
	out_fluxes,
	time_symbol,
	startValues
){
  accumulated_fluxes_startvector <- rep(0,length(in_fluxes)+length(internal_fluxes)+length(out_fluxes))
  names(accumulated_fluxes_startvector) <-c(
    unlist(
      lapply(
        in_fluxes,
        function(flux){
          paste0('accumulated_influx','.',flux@destinationName)
        }
      )
    )
    ,
    unlist(
      lapply(
        internal_fluxes,
        function(flux){
          paste0(
            'accumulated_internal_flux',
            '.',
            src_to_dest_string(flux@sourceName,flux@destinationName)
          )
        }
      )
    )
    ,
    unlist(
      lapply(
        out_fluxes,
        function(flux){
          paste0('accumulated_outflux','.',flux@sourceName)
        }
      )
    )
  )
  accumulated_fluxes_startvector
  ydot=ydot_maker(
    in_fluxes,
		internal_fluxes,
		out_fluxes,
		time_symbol,
		names=names(startValues)
  )
  list(ydot=ydot,startValues=c(startValues,accumulated_fluxes_startvector))
}
#------------------------------------------------------
#test.ydotCreation=function(){
  ks<-c(k_barrel=0.1,k_bottle=0.2)
  k=0.2
  intfs=InternalFluxList_by_PoolName(
    list(
        InternalFlux_by_PoolName(
            sourceName='barrel'
            ,destinationName='glass'
            ,func=function(barrel,t){
                # just a linear donor dependent flux 
                # although we could use the glass argument 
                k*barrel 
            }
        )
    )
  )
    
  ofs=OutFluxList_by_PoolName(
        c(
            OutFlux_by_PoolName(
                sourceName='barrel'
                ,func=function(barrel,t){
                    k*barrel 
                    # just a linear donor dependent 
                    # flux the second argument is fake but here for the test
                }
            )
        )
  )
  ifs=InFluxList_by_PoolName(
        c(
            InFlux_by_PoolName(
                destinationName='barrel'
                ,func=function(barrel,glass,t){
                    # ignore arguments
                    #res=(barrel+glass)*(1+sin(t))/100
                    1
                }
            )
        )
  )
 
 obn<- UnBoundNonLinDecompOp_by_PoolNames(
   internal_fluxes=intfs
   ,out_fluxes=ofs
   ,timeSymbol='t'
 )
    
  ivp <- IVP_maker( 
    in_fluxes=ifs,
	  internal_fluxes=intfs,
	  out_fluxes=ofs,
	  time_symbol='t',
	  startValues=c(barrel=1,bottle=2,glass=5)
  )
  #print(ydot(1,startValues,ks))
  times<-1:100
  mod=GeneralModel(
        t=times
        ,A=obn
        ,ivList=iv 
        ,inputFluxes=ifs
        ,timeSymbol='t'
  )
   
  print(
    deSolve::lsoda(y=ivp$startValues,times=times,func=ivp$ydot,parms=ks)
  )
#}

#------------------------------------------------------
#test.ydotCreation()

