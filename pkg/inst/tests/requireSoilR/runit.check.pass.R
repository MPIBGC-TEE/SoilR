require('SoilR')
#
# vim:set ff=unix expandtab ts=2 sw=2:
Result<-setClass(
  "Result",
  slots=c(
    noException="list",
    notRunning="list",
    tested="list"
  ),
  prototype=list(
    noException=list(),
    notRunning=list(),
    tested=list()
  )
)
test.check.pass=function(){
	#create a vector of all the function names
	X <- lsf.str('package:SoilR')
	#create a helper that checks for the pass argument in the functions 
	#parameter list
	has_pass <-function(x){"pass" %in% names(formals(x))}
	ind=as.vector(sapply(X,has_pass))
	#print(ind)
	#extract the functions that have such an argument
  pe('ind')
	Xpass=X[ind]
	#now we check if the "pass" argumet is handled correctly by those functions
	#To this end we define a caller that keeps book over the actually tested
	#functions and the success 

	passCaller <- function(c,l){
		cl=as.list(c)
		name=cl[[1]]
		print(cl)
		#first check that without pass argument an exception is thrown
    #only for more informative output we check manually before we use checkException
    #we want a message if the code does > n o t <  break as intended
    if(class(try(eval(c),silent=T))!="try-error"){
       l@noException<- append(l@noException,name)
    }
		# now extend the call by an additional pass=TRUE argument
		c2 <- as.call(append(cl,expression(pass=TRUE)))
  	#and check that the pass argument suffices to make the code run
    if(class(try(eval(c2),silent=T))=="try-error"){
  		l@notRunning=append(l@notRunning,name)
    }
    l@tested=append(l@tested,name)
    return(l)
	}  
	## =Now we call all the functions with the pass argument and store the names in a list that will we later check
	## for completeness
	#-----------------------------------------------------------------------------------------
	l=Result()
	load("../../data/C14Atm_NH.rda")
	load("../../data/HarvardForest14CO2.rda")
	years=seq(1801,2010,by=0.5) # use a wrong time interval intentionally

	l=passCaller(call("GaudinskiModel14",t=years,ks=c(kr=1/3,koi=1/1.5,koeal=1/4,koeah=1/80,kA1=1/3,kA2=1/75,kM=1/110),inputFc=C14Atm_NH),l)
	#-----------------------------------------------------------------------------------------
  t_start=0 
  t_end=10 
  tn=50
  timestep=(t_end-t_start)/tn 
  t=seq(t_start,t_end,timestep) 
	## create an alternative time vector to provoke an exception 
	t_fault=seq(t_start-10,t_end,timestep) 
  n=3
  At=new("BoundLinDecompOp",
  t_start,
  t_end,
  function(t0){
        matrix(nrow=n,ncol=n,byrow=TRUE,
          c(-0.2,    0,    0, 
             0  , -0.3,    0,   
             0,      0,   -0.4)
        )
  }
  ) 
  
  c0=c(0.5, 0.5, 0.5)
  #constant inputrate
  inputFluxes=TimeMap.new(
    t_start,
    t_end,
    function(t0){matrix(nrow=n,ncol=1,c(0.0,0,0))}
  ) 
  l=passCaller(call("GeneralModel",t_fault,At,c0,inputFluxes),l)
	#-----------------------------------------------------------------------------------------
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
  l=passCaller(call("Model_14",t=t_fault,A=At,ivList=c0,initialValF=F0,inputFluxes=inputFluxes,inputFc=Fc,c14DecayRate=k),l)
	#-----------------------------------------------------------------------------------------
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
  l=passCaller(call("GeneralModel_14",t=t_fault,A=At,ivList=c0,initialValF=F0,inputFluxes=inputFluxes,inputFc=Fc,di=k),l)
	#-----------------------------------------------------------------------------------------
  # create the operator for a two pool serial model
  # according to our new general definition
  # 
  t_start=0
  t_end=10
  tn=3e1
  timestep=(t_end-t_start)/tn
  t=seq(t_start,t_end,timestep)
	## create an alternative time vector to provoke an exception 
	t_fault=seq(t_start-10,t_end,timestep) 
  nr=2
  # define the transfer functions for the model
  # we could compile them to a matrix valued
  # Function of C and t since they will be 
  # applied in a linear way on the output vector.
  # but we rather store them in an indexed list 
  # (as a sparse matrix) which also has some 
  # implementational benefits because the single
  # functions are easier to retrieve from the operator
  # if needed.
  alpha=list()
  alpha[["1_to_2"]]=function(C,t){
    1#all stuff is transmitted
  }


  k1=3/5
  k2=3/5
  f=function(C,t){
    # in this case the application of f can be expressed by a matrix multiplication
    # f(C,t)=N C
    # furthermorde the matrix N is actually completely linear and even constant
    N=matrix( 
       nrow=nr,
       ncol=nr,
       c(
          k1,    0,  
          0  ,  k2  
       )
    )
    # so we can write f(C,t)  as a Matrix product
    # note however that we could anything we like with the components
    # of C here. 
    # The only thing to take care of is that we release a vector of the same
    # size as C
    return(N%*%C)
  }
  fac=2e2
  
  inputrates=new("TimeMap",t_start,t_end,function(t){return(matrix(
    nrow=nr,
    rep(
      c(
        2*fac,  0*fac
      ),
      length(t)
    )
  ))})
  c0= c( fac, 0 )
  A=new("TransportDecompositionOperator",t_start,t_end,nr,alpha,f)
  l=passCaller(call("GeneralNlModel",t_fault,A,c0,inputrates),l)
	
	#-----------------------------------------------------------------------------------------
        times=seq(0,20,by=0.1)
	# to provoke an exeption we insert a negative decomposition rate
	ks=c(k1=-0.8,k2=0.00605)
	l=passCaller(call("ICBMModel",t=times, ks=ks,h=0.125, r=1,    c0=c(0.3,4.11),  In=0.19+0.095),l) #+N +Straw
	#-----------------------------------------------------------------------------------------
	t_start=0 
  t_end=10 
  tn=50
  timestep=(t_end-t_start)/tn 
	t=seq(t_start,t_end,timestep) 
	## create an alternative time vector to provoke an exception 
	t_fault=seq(t_start-10,t_end,timestep) 
	In=data.frame(t,rep(100,length(t)))
	t=seq(t_start,t_end,timestep) 
  k=0.8
  C0=100
  l=passCaller(call("OnepModel",t_fault,k,C0,In),l)
	#-----------------------------------------------------------------------------------------

	load("../../data/C14Atm_NH.rda")
  years=seq(1901,2009,by=0.5)
  years_fault=seq(1901,2019,by=0.5)
  LitterInput=700 
  
	l=passCaller(call("OnepModel14",t=years_fault,k=1/10,C0=500, F0=0,In=LitterInput, inputFc=C14Atm_NH),l)
      
	#-----------------------------------------------------------------------------------------
  t_start=0 
  t_end=10 
  tn=50
  timestep=(t_end-t_start)/tn 
  t=seq(t_start,t_end,timestep) 
  k=TimeMap.new(t_start,t_end,function(times){c(-0.5,-0.2,-0.3)})
  c0=c(1, 2, 3)
  #constant inputrates
  inputrates=TimeMap.new(
      t_start+10,#we introduce a to short definition time for the inputrates to provoke the erro
      t_end,
      function(t){matrix(nrow=3,ncol=1,c(1,1,1))}
  ) 
  l=passCaller(call("ParallelModel",t,k,c0,inputrates),l)
	#-----------------------------------------------------------------------------------------
	t=0:500 
	In=data.frame(t,rep(1.7,length(t)))
	FYM=data.frame(t,rep(1.7,length(t)))
	t_faulty=0:600 
	l=passCaller(call("RothCModel",t_faulty,In=In,FYM=FYM),l)
    
	#-----------------------------------------------------------------------------------------
	t_start=0 
  t_end=10 
  tn=50
  timestep=(t_end-t_start)/tn 
  t=seq(t_start,t_end,timestep) 
	t_fault=seq(t_start-10,t_end,timestep) 
  ks=c(k1=0.8,k2=0.4,k3=0.2)
  C0=c(C10=100,C20=150, C30=50)
  In = 60
  
  #Temp=rnorm(t,15,1)
  Temp=rep(15,length(t))
  TempEffect=data.frame(t,fT.Daycent1(Temp))

  l=passCaller(call("ThreepFeedbackModel",t=t_fault,ks=ks,a21=0.5,a12=0.1,a32=0.2,a23=0.1,C0=C0,In=In,xi=TempEffect),l)
  l=passCaller(call("SeriesLinearModel",t=t_fault,ki=ks,m.pools=3,Tij=c(0.5,0.2),C0=C0,In=In,xi=TempEffect),l)
	#-----------------------------------------------------------------------------------------
	
	load("../../data/C14Atm_NH.rda")
  years=seq(1901,2009,by=0.5)
  years_fault=seq(1901,2019,by=0.5)
  LitterInput=700 
  l=passCaller(
	  call("ThreepFeedbackModel14",t=years_fault,ks=c(k1=1/2.8, k2=1/35, k3=1/100),C0=c(200,5000,500), F0_Delta14C=c(0,0,0),In=LitterInput, a21=0.1,a12=0.01,a32=0.005,a23=0.001,inputFc=C14Atm_NH),
	  l
  )
  l=passCaller( call("SeriesLinearModel14",t=years_fault,ki=c(k1=1/2.8, k2=1/35, k3=1/100),C0=c(200,5000,500),m.pools=3, F0_Delta14C=c(0,0,0),In=LitterInput, Tij=c(0.5,0.1),inputFc=C14Atm_NH), l)

	#-----------------------------------------------------------------------------------------
	t_start=0 
  t_end=10 
  tn=50
  timestep=(t_end-t_start)/tn 
  t=seq(t_start,t_end,timestep) 
	t_fault=seq(t_start-10,t_end,timestep) 
	In=data.frame(t,rep(1.7,length(t)))
  l=passCaller(call("ThreepParallelModel",t=t_fault,ks=c(k1=0.5,k2=0.2,k3=0.1),C0=c(c10=100, c20=150,c30=50),In=In,gam1=0.7,gam2=0.1,xi=0.5),l)

	#-----------------------------------------------------------------------------------------
	load("../../data/C14Atm_NH.rda")
	years=seq(1901,2009,by=0.5)
	years_fault=seq(1901,2019,by=0.5)
	LitterInput=700 
    
	l=passCaller(call("ThreepParallelModel14",t=years_fault,ks=c(k1=1/2.8, k2=1/35, k3=1/100),C0=c(200,5000,500), F0_Delta14C=c(0,0,0),In=LitterInput, gam1=0.7, gam2=0.1, inputFc=C14Atm_NH,lag=2),l)

	#-----------------------------------------------------------------------------------------
	
	load("../../data/C14Atm_NH.rda")
  years=seq(1901,2009,by=0.5)
  years_fault=seq(1901,2019,by=0.5)
  LitterInput=700 
  
	l=passCaller( call("ThreepFeedbackModel14",t=years_fault,ks=c(k1=1/2.8, k2=1/35, k3=1/100),C0=c(200,5000,500), F0_Delta14C=c(0,0,0),In=LitterInput, a21=0.1,a12=0.01,a32=0.005,a23=0.001,inputFc=C14Atm_NH), l)
	#-----------------------------------------------------------------------------------------

	t_start=0 
  t_end=10 
  tn=50
  timestep=(t_end-t_start)/tn 
  t=seq(t_start,t_end,timestep) 
	t_fault=seq(t_start-10,t_end,timestep) 
  ks=c(k1=0.8,k2=0.4,k3=0.2)
  C0=c(C10=100,C20=150, C30=50)
	In=data.frame(t,rep(50,length(t)))
	l=passCaller(call("ThreepSeriesModel",t=t_fault,ks=ks,a21=0.5,a32=0.2,C0=C0,In=In,xi=fT.Q10(15)),l)
	#-----------------------------------------------------------------------------------------
	load("../../data/C14Atm_NH.rda")
  years=seq(1901,2009,by=0.5)
	years_fault=seq(1901,2019,by=0.5)
  LitterInput=700 
  l=passCaller( call("ThreepSeriesModel14",t=years_fault,ks=c(k1=1/2.8, k2=1/35, k3=1/100),C0=c(200,5000,500), F0_Delta14C=c(0,0,0),In=LitterInput, a21=0.1, a32=0.01,inputFc=C14Atm_NH), l)
	#-----------------------------------------------------------------------------------------
	times=seq(0,20,by=0.1)
	times_fault=seq(0,30,by=0.1) 
  ks=c(k1=0.8,k2=0.00605)
  C0=c(C10=5,C20=5)
    
  Temp=rnorm(times,15,2)
  WC=runif(times,10,20)
  TempEffect=data.frame(times,fT=fT.Daycent1(Temp))
  MoistEffect=data.frame(times, fW=fW.Daycent2(WC)[2])
  
  Inmean=1
  #InRand=data.frame(times,Random.inputs=rnorm(length(times),Inmean,0.2))
  InSin=data.frame(times,Inmean+0.5*sin(times*pi*2))
  l=passCaller(call("TwopParallelModel",t=times_fault,ks=ks,C0=C0,In=InSin,gam=0.9, xi=(fT.Daycent1(15)*fW.Demeter(15))),l)
	l=passCaller(call("TwopSeriesModel",t=times_fault,ks=ks,a21=0.2*ks[1],C0=C0,In=InSin, xi=(fT.Daycent1(15)*fW.Demeter(15))),l)
	l=passCaller(call("TwopFeedbackModel",t=times_fault,ks=ks,a21=0.2*ks[1],a12=0.5*ks[2],C0=C0, In=InSin,xi=MoistEffect),l)
	#-----------------------------------------------------------------------------------------
	load("../../data/C14Atm_NH.rda")
  years=seq(1901,2009,by=0.5)
	years_fault=seq(1901,2019,by=0.5)
  LitterInput=data.frame(years,rep(700,length(years)))
	l=passCaller(call("TwopFeedbackModel14",t=years_fault,ks=c(k1=1/2.8, k2=1/35),C0=c(200,5000), F0_Delta14C=c(0,0),In=LitterInput, a21=0.1,a12=0.01,inputFc=C14Atm_NH),l)
	#-----------------------------------------------------------------------------------------
	load("../../data/C14Atm_NH.rda")
  years=seq(1901,2009,by=0.5)
	years_fault=seq(1901,2019,by=0.5)
  LitterInput=data.frame(years,rep(700,length(years)))
	l=passCaller(call("TwopParallelModel14",t=years_fault,ks=c(k1=1/2.8, k2=1/35),C0=c(200,5000), F0_Delta14C=c(0,0),In=LitterInput, gam=0.7,inputFc=C14Atm_NH,lag=2),l)
	#-----------------------------------------------------------------------------------------
	load("../../data/C14Atm_NH.rda")
  years=seq(1901,2009,by=0.5)
	years_fault=seq(1901,2019,by=0.5)
  LitterInput=data.frame(years,rep(700,length(years)))
  l=passCaller(call("TwopSeriesModel14",t=years_fault,ks=c(k1=1/2.8, k2=1/35),C0=c(200,5000), F0_Delta14C=c(0,0),In=LitterInput, a21=0.1,inputFc=C14Atm_NH),l)
	#-----------------------------------------------------------------------------------------
  years=seq(0,50,0.1) 
  years_fault=seq(0,60,0.1) 
  C0=rep(100,7)
  xi=data.frame(years,rep(1,length(years)))
  l=passCaller(call("YassoModel",t=years_fault,C0=C0,xi=xi),l)
  
	#-----------------------------------------------------------------------------------------
  years=seq(0,50,0.1) 
  years_fault=seq(0,60,0.1) 
  C0=rep(100,5)
  LitterInput=data.frame(years,rep(0,length(years)))
  l=passCaller(call("Yasso07Model",t=years_fault,C0=C0,In=LitterInput),l)
  
	#-----------------------------------------------------------------------------------------
  years=seq(0,50,0.1) 
  years_fault=seq(0,60,0.1) 
  n=5
  C0=rep(100,n)
  LitterInput<-BoundInFlux(data.frame(years,rep(0,length(years))))
  op<-ConstLinDecompOp(diag(rep(-1,n)))
  l<-passCaller(call("Model",A=op,t=years_fault,ivList=C0,inputFluxes=LitterInput),l)
  
	#----------------------------------------------------------------------------------------  -
  st=FALSE
	if (length(setdiff(Xpass,l@tested))!=0){
    cat(
      paste("not all functions using the pass argument have been tested. \nThe untested functions are:\n",
        paste(setdiff(Xpass,l@tested),collapse=", "),
        "\n\n"
      )
    )
    st<-TRUE
  }
	if (length(l@noException)>0){
    cat( 
      paste(
        "Not all functions produce the required exception for wrong input if called without the pass argument.\nThe  funtions are:\n",
        paste(l@noException,collapse=", "),
        "\n\n"
      )
    )
    st<-TRUE
  } 
	if (length(l@notRunning)>0){
    cat(
      paste("Not all functions can be made run with corrupted input by calling them with the pass argument.\nThe  funtions are:\n",
        paste(unlist(l@notRunning),collapse=", "),
        "\n\n"
      )
    )
    st<-TRUE
  } 
  if (st){stop()}
}
