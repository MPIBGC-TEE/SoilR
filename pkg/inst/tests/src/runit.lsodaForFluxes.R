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
    num_in_fluxes <- as.numeric(in_fluxes,y,t,time_symbol)
    num_internal_fluxes <- as.numeric(internal_fluxes,y,t,time_symbol)
    content_change<- function(name,y,t,time_symbol){
      in_to_name <- ifelse(name %in% names(num_in_fluxes),num_in_fluxes[[name]],0)
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
      in_to_name+internal_to_name
    }
    # ydot is a vector 
    # for every component y_i we have
    # dy_i / d_t =  incomeing_to_i                -         outgoing_from_i
    # dy_i / d_t =  sum(in_to_i)+sum(intern_to_i) -  (sum(internal_from_i)+sum(out_from_i))       

    ydot_num <- as.numeric(
      lapply(
        names,
        function(name) content_change(name,y,t,time_symbol)
      )
    )
    #with(as.list(c(parms,y)),{
    #  ydot<- -c(
    #    k_barrel*barrel,
    #    k_bottle*bottle,
    #    k_bottle*glass
    #  )

    #  outfluxes<-c(barrel=k_barrel*barrel,bottle=k_bottle*bottle)
    #  list(ydot,outfluxes=outfluxes)
    #})
    list(ydot_num,influxes=num_in_fluxes,internal_fluxes=num_internal_fluxes)
  }
  ydot
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
 
  startValues=c(barrel=1,bottle=2,glass=3) #vector
  ydot=ydot_maker(
    in_fluxes=ifs,
		internal_fluxes=intfs,
		out_fluxes=ofs,
		time_symbol='t',
		names=names(startValues)
  )

  #print(ydot(1,startValues,ks))
  times<-1:100
  print(
    deSolve::lsoda(y=startValues,times=times,func=ydot,parms=ks)
  )
#}

#------------------------------------------------------
#test.ydotCreation()

