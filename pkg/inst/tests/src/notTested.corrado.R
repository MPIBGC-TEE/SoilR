#--------------------------------------------------------------
test.term1=function(){
    source("functions_corrado.R")  
    C_am_0	=1
    C_as_0	=1
    C_bm_0	=1
    C_bs_0	=1
    C_fw_0	=1
    C_acw_0	=1
    C_bcw_0	=1
    C_mic_0	=1
    C_slo_0	=1
    C_pas_0	=1
    N_am_0	=1
    N_as_0	=1
    N_bm_0	=1
    N_bs_0	=1
    N_fw_0	=1
    N_acw_0	=1
    N_bcw_0	=1
    N_mic_0	=1
    N_slo_0	=1
    N_pas_0	=1
    N_ino_0	=1

  term1=term1(
    C_am_0
    ,N_am_0
    ,C_mic_0
    ,N_mic_0
    ,N_as_0
    ,C_as_0
    ,C_slo_0
    ,N_slo_0
    ,C_bm_0
    ,N_bm_0
    ,C_bs_0
    ,N_bs_0
    ,C_fw_0
    ,N_fw_0
    ,C_acw_0
    ,N_acw_0
    ,C_bcw_0
    ,N_bcw_0
    ,C_pas_0
    ,N_pas_0
  )
  print_loud(term1)
  checkTrue(length(term1)==1)
  checkTrue(class(term1)=='numeric')
}
#--------------------------------------------------------------
test.phi_mn=function(){
    source("functions_corrado.R")  
    C_am_0	=1
    C_as_0	=1
    C_bm_0	=1
    C_bs_0	=1
    C_fw_0	=1
    C_acw_0	=1
    C_bcw_0	=1
    C_mic_0	=1
    C_slo_0	=1
    C_pas_0	=1
    N_am_0	=1
    N_as_0	=1
    N_bm_0	=1
    N_bs_0	=1
    N_fw_0	=1
    N_acw_0	=1
    N_bcw_0	=1
    N_mic_0	=1
    N_slo_0	=1
    N_pas_0	=1
    N_ino_0	=1

  res=phi_mn_scalar(
    C_am_0
    ,N_am_0
    ,C_mic_0
    ,N_mic_0
    ,N_as_0
    ,C_as_0
    ,C_slo_0
    ,N_slo_0
    ,C_bm_0
    ,N_bm_0
    ,C_bs_0
    ,N_bs_0
    ,C_fw_0
    ,N_fw_0
    ,C_acw_0
    ,N_acw_0
    ,C_bcw_0
    ,N_bcw_0
    ,C_pas_0
    ,N_pas_0
    ,N_ino_0	
    ,t=0
  )
  print_loud(res)
  checkTrue(length(res)==1)
  checkTrue(class(res)=='numeric')
}
#--------------------------------------------------------------
#test.phi_mn_with_initialValues=function(){
#  #library(SoilR)
#  print(getwd())
#  source("corrado.R")  
#  
#  #### prepare some arguments
#    # set some initial values
#    C_am_0	=1
#    C_as_0	=1
#    C_bm_0	=1
#    C_bs_0	=1
#    C_fw_0	=1
#    C_acw_0	=1
#    C_bcw_0	=1
#    C_mic_0	=1
#    C_slo_0	=1
#    C_pas_0	=1
#    N_am_0	=1
#    N_as_0	=1
#    N_bm_0	=1
#    N_bs_0	=1
#    N_fw_0	=1
#    N_acw_0	=1
#    N_bcw_0	=1
#    N_mic_0	=1
#    N_slo_0	=1
#    N_pas_0	=1
#    N_ino_0	=1
#    
#    X_0=numeric(21)
#    X_0[[1]]	<-C_am_0 
#    X_0[[2]]	<-C_as_0 
#    X_0[[3]]	<-C_bm_0 
#    X_0[[4]]	<-C_bs_0 
#    X_0[[5]]	<-C_fw_0 
#    X_0[[6]]	<-C_acw_0
#    X_0[[7]]	<-C_bcw_0
#    X_0[[8]]	<-C_mic_0
#    X_0[[9]]	<-C_slo_0
#    X_0[[10]]	<-C_pas_0
#    X_0[[11]]	<-N_am_0 
#    X_0[[12]]	<-N_as_0 
#    X_0[[13]]	<-N_bm_0 
#    X_0[[14]]	<-N_bs_0 
#    X_0[[15]]	<-N_fw_0 
#    X_0[[16]]	<-N_acw_0
#    X_0[[17]]	<-N_bcw_0
#    X_0[[18]]	<-N_mic_0
#    X_0[[19]]	<-N_slo_0
#    X_0[[20]]	<-N_pas_0
#    X_0[[21]]	<-N_ino_0
#
# #### call the function
#    res<-phi_mn(X_0,t=0)
#    myres=5
#    checkEquals(res,myres) 
#}

#--------------------------------------------------------------
test.corrados_next_function=function(){
  print('nothing can go wrong yet')
  source("corrado.R")  
  #print(internal_fluxes)
  #### prepare some arguments
    # set some initial values
    C_am_0	=1
    C_as_0	=1
    C_bm_0	=1
    C_bs_0	=1
    C_fw_0	=1
    C_acw_0	=1
    C_bcw_0	=1
    C_mic_0	=1
    C_slo_0	=1
    C_pas_0	=1
    N_am_0	=1
    N_as_0	=1
    N_bm_0	=1
    N_bs_0	=1
    N_fw_0	=1
    N_acw_0	=1
    N_bcw_0	=1
    N_mic_0	=1
    N_slo_0	=1
    N_pas_0	=1
    N_ino_0	=1
    
    X_0=numeric(21)
    X_0[[1]]	<-C_am_0 
    X_0[[2]]	<-C_as_0 
    X_0[[3]]	<-C_bm_0 
    X_0[[4]]	<-C_bs_0 
    X_0[[5]]	<-C_fw_0 
    X_0[[6]]	<-C_acw_0
    X_0[[7]]	<-C_bcw_0
    X_0[[8]]	<-C_mic_0
    X_0[[9]]	<-C_slo_0
    X_0[[10]]	<-C_pas_0
    X_0[[11]]	<-N_am_0 
    X_0[[12]]	<-N_as_0 
    X_0[[13]]	<-N_bm_0 
    X_0[[14]]	<-N_bs_0 
    X_0[[15]]	<-N_fw_0 
    X_0[[16]]	<-N_acw_0
    X_0[[17]]	<-N_bcw_0
    X_0[[18]]	<-N_mic_0
    X_0[[19]]	<-N_slo_0
    X_0[[20]]	<-N_pas_0
    X_0[[21]]	<-N_ino_0

  for (fn in names(internal_fluxes)){
          f=internal_fluxes[[fn]]
          val=f(X_0,0)
          l=length(val)
          t=class(val)
          if (l!=1 | t!='numeric'){
                print(paste('InternalFlux:',fn,'length:', l))
                stop()
          } 
  }
}

#--------------------------------------------------------------
test.InternalFluxe_by_PoolName<-function(){
  source('corrado.R')
  C_am_0	=1
  C_as_0	=1
  C_bm_0	=1
  C_bs_0	=1
  C_fw_0	=1
  C_acw_0	=1
  C_bcw_0	=1
  C_mic_0	=1
  C_slo_0	=1
  C_pas_0	=1
  N_am_0	=1
  N_as_0	=1
  N_bm_0	=1
  N_bs_0	=1
  N_fw_0	=1
  N_acw_0	=1
  N_bcw_0	=1
  N_mic_0	=1
  N_slo_0	=1
  N_pas_0	=1
  N_ino_0	=1

  f<-internal_fluxes_by_PoolName[["C_am->C_mic"]]
  print_loud(
      f(
           C_am_0
          ,N_am_0
          ,C_mic_0
          ,N_mic_0
          ,N_as_0
          ,C_as_0
          ,C_slo_0
          ,N_slo_0
          ,C_bm_0
          ,N_bm_0
          ,C_bs_0
          ,N_bs_0
          ,C_fw_0
          ,N_fw_0
          ,C_acw_0
          ,N_acw_0
          ,C_bcw_0
          ,N_bcw_0
          ,C_pas_0
          ,N_pas_0
          ,N_ino_0
          ,0
      )
  )
}
