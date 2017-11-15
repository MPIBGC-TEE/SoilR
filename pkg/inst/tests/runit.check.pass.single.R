#
# vim:set ff=unix expandtab ts=2 sw=2:
  t_start=1960
  t_end=2010
  tn=220
  timestep=(t_end-t_start)/tn 
  t=seq(t_start,t_end,timestep) 
  t_fault=seq(t_start-10,t_end,timestep) 
  n=3
  At=new(Class="BoundLinDecompOp",
    t_start,
    t_end,
    function(t0){
          matrix(nrow=n,ncol=n,byrow=TRUE,
            c(-1,    0.1,    0, 
               0.5  , -0.4,    0,   
               0,    0.2,   -0.1)
          )
    }
  ) 
   
  c0=c(100, 100, 100)
  F0=ConstFc(c(0,10,10),"Delta14C")
  #constant inputrate
  inputFluxes=new(
    "TimeMap",
    t_start,
    t_end,
    function(t0){matrix(nrow=n,ncol=1,c(10,10,10))}
  ) 
  # we have a dataframe representing the C_14 fraction 
  # note that the time unit is in years and the fraction is given in
  # the Absolute Fraction Modern format.
  # This means that all the other data provided are assumed to have the same value
  # This is especially true for the decay constants to be specified later
  load("../../data/C14Atm_NH.rda")
  Fc=BoundFc(C14Atm_NH,format="Delta14C")
  # add the C14 decay to the matrix which is done by a diagonal matrix which does not vary over time
  # we assume a half life th=5730 years
  th=5730
  k=log(0.5)/th #note that k is negative and has the unit y^-1
  m1 <- Model_14(t=t_fault,A=At,ivList=c0,initialValF=F0,inputFluxes=inputFluxes,inputFc=Fc,c14DecayRate=k,pass=TRUE)
	#-----------------------------------------------------------------------------------------
