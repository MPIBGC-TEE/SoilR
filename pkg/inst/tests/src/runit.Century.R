test.ExtractionOfConstantLinDecompOpAndXi<-function(){
    LNcorn <- 0.17/0.004 # Values for corn clover reported in Parton et al. 1987
    xi_func <- function(t){2+sin(t)}
    t_model1 <- seq(0,5200,1) #
    lag <- 1
    Ex1 <- CenturyModel(
      t_model1,
			LN=0.5,
			Ls=0.1,
			In=0.1,
			xi=xi_func,
			xi_lag=lag
    )
    Ct <- getC(Ex1)
    Rt <- getReleaseFlux(Ex1)
    # now extract the operator (all model objects can do this)
    op <- getDecompOp(Ex1)
    
    # and from the operator the matrix 
    print(getConstantCompartmentalMatrix(op))
    
    # This will (intentionally) only work for certain operators that have this
    # method which are at the moment only two classes: - "ConstLinDecompOp" -
    # "ConstLinDecompOpWithLinearScaleFactor"
    #
    # Therefore it is also questionable to make this a mehtod of the Model
    # class so that you could call it directly on Ex1 (Unless we create a
    # special model class the model does not know if the method call makes
    # sense because at the moment it works with all operators


    # you can also extract the automatically created ScalarTimeMap object
    tm <- getLinearScaleFactor(op)
    print(tm)
    # from which you can get a normal function as usual
    xi_func_extracted <- getFunctionDefinition(tm) 
    # Note that this incorporates a possible lag 
    # (If you want to check  plot it together with the original xi_func.)


    # You can also extract the ConstLinDecomp object (the constant part of the operator)
    clo <- getConstLinDecompOp(op) #(and call the getConstantCompartmentalMatrix on this 
    # object yourself
    #From the clo object you can also extract the fluxrates 
    cintflrs_by_ind <- getConstantInternalFluxRateList_by_PoolIndex(clo)
    coflrs_by_ind <- getConstantOutFluxRateList_by_PoolIndex(clo)


    
    # The influxes are not part of the compartmental operator but we can get
    # them back from the model object.  
    # In the case of constant influxes this is pretty easy even if they had
    # been originally given as vectors but for all nonautonomous cases  our
    # traditional vector valued functions are unfortunate as it is more or less
    # impossible to know if some components of a vector valued function are
    # zero for all times and SoilR would have to assume the most general case
    # that all components are non zero....
    # To achieve structural transparency it is much better to have a 'vector
    # of functions', which is the reason I changed the way CenturyModel
    # defines the influxes. This will scale also to nonautonomous fluxes. It
    # will reach its limits though for nonlinear fluxes as it turns out that
    # for these the only way that preserves the full structural information is
    # to use scalar functions that do not even have vector arguments.  I have
    # not implemented this yet in 'Century' because I did not know how the
    # pools are called in Century, but it will be shown in Corrados example.
    ifls <- getInFluxes(Ex1)
    cifls_by_ind<-ConstantInFluxList_by_PoolIndex(ifls)

    # now we can finally plot the connections This is very much ad hoc still
    # but it shows how it has to be done. It will finally be a
    # method of a new model but this requires the model object to be ablte to
    # transform its influxes and compartmental operator to flux lists with the
    # challanges mentioned above
    internalConnections<-lapply(
        cintflrs_by_ind
        ,function(flr){
            tuple(
                as.integer(flr@sourceIndex)
                ,as.integer(flr@destinationIndex)
            )
        }
    )
    outBoundConnections<- lapply(
        coflrs_by_ind
        ,function(fl){ as.integer(fl@sourceIndex) }
    )
    inBoundConnections<-lapply(
        cifls_by_ind
        ,function(fl){ as.integer(fl@destinationIndex) }
    )

    plotPoolGraphFromTupleLists(internalConnections,inBoundConnections,outBoundConnections)

    # You can also reindex the flux rates by PoolNames if you provide a vector
    # of poolNames where the names are almost free to choose as long as they
    # would qualify as variable names in R (which is tested). 
    # This is relevan for the other direction from PoolName to PoolIndex since for
    # nonlinear models this includes the translation of scalar function arguments
    # to vector arguments
    poolNames <- c(
      "Structural_litter",
      "Metabolic_litter",
      "Active_SOM",
      "Slow_SOM",
      "Passive_SOM"
    )
    cintflrs_by_name <- by_PoolName(cintflrs_by_ind,poolNames)
    coflrs_by_name <- by_PoolName(coflrs_by_ind,poolNames)
    cifls_by_name<-by_PoolName(cifls_by_ind,poolNames)

    internalConnections<-lapply(
        cintflrs_by_name
        ,function(flr){
            tuple(
                as.character(flr@sourceName)
                ,as.character(flr@destinationName)
            )
        }
    )
    outBoundConnections<- lapply(
        coflrs_by_name
        ,function(fl){ as.character(fl@sourceName) }
    )
    inBoundConnections<-lapply(
        cifls_by_name
        ,function(fl){ as.character(fl@destinationName) }
    )

    plotPoolGraphFromTupleLists(internalConnections,inBoundConnections,outBoundConnections)
    
    # We could even build a model of the new kind. At the time of writing the only
    # available model class is SymbolicModel_by_poolName, which is in general nonlinear
    # nonautonomous. So we could translate our lists of constant rates and fluxes to 
    # (formal) functions of time and the statevariables as in 
    # WangThreePoolNonAutonomous_sym.R
    # Future Versions of SoilR can automate this translation.

}

test.withXiDataframe<-function(){
    ## second example with data
    xi_func <- function(t){2+sin(t)}
    t_data <- 100:4000
    xi_df <- data.frame(t=t_data,xi=xi_func(t_data))
    lag <- 1
    t_model2 <-  t_data 
    Ex2 <- CenturyModel(
      t_model2,
			LN=0.5,
			Ls=0.1,
			In=0.1,
			xi=xi_df,
			xi_lag=lag
    )
    Ct <- getC(Ex2)
    Rt <- getReleaseFlux(Ex2)

}

test.oldExample<-function(){
  t <- seq(0,52*200,1) #200 years
  LNcorn <- 0.17/0.004 # Values for corn clover reported in Parton et al. 1987
  Ex<-CenturyModel(
    t,
		LN=0.5,
		Ls=0.1,
		In=0.1
  )
  Ct <- getC(Ex)
  Rt <- getReleaseFlux(Ex)

  matplot(
    t,
    Ct,
    type="l",
    col=1:5,
    lty=1,
    ylim=c(0,max(Ct)*2.5),
    ylab=expression(paste("Carbon stores (kg C", ha^-1,")")),
    xlab="Time (weeks)"
  )
  lines(
    t,
    rowSums(Ct),
    lwd=2
  )
  legend(
    "topright",
    c(
      "Structural litter",
      "Metabolic litter",
      "Active SOM",
      "Slow SOM",
      "Passive SOM",
      "Total Carbon"
    ),
    lty=1,
    lwd=c(rep(1,5),2),
    col=c(1:5,1),
    bty="n"
  )

  matplot(
    t,
		Rt,
		type="l",
		lty=1,
		ylim=c(0, max(Rt)*3),
		ylab="Respiration (kg C ha-1 week-1)",
		xlab="Time"
  )
  lines(
    t,
    rowSums(Rt),
    lwd=2
  )
  legend(
    "topright",
		c(
      "Structural litter",
		  "Metabolic litter",
		  "Active SOM",
      "Slow SOM",
      "Passive SOM",
      "Total Respiration"
    ),
    lty=1,
    lwd=c(rep(1,5),2),
    col=c(1:5,1),
    bty="n"
  )
}

test.user.Example<-function(){
	C0 <- c(1, 2, 15, 8, 3)
	time <- seq(0, 1, 1/12)
	temp <- c(0, -7, 2, 5, 11, 12, 16, 13, 13, 8, 4, 3, 2)
	rainfall <- c(63, 12, 33, 22, 24, 80, 52, 133, 57, 76, 55, 92, 71)
	pet <- c(35, 16, 48, 59, 115, 110, 139, 114, 103, 71, 47, 46, 42)
	xi <- fT.Century1(temp) * fW.Century(rainfall, pet)
	xi <- data.frame(time, xi=xi[1:length(time)])
	
	Century <- CenturyModel(
    t = time,
		C0 = C0,
		LN = 0.5,
		Ls = 0.1,
		In = 0,
		clay = 0.1,
		silt = 0.2,
		xi = xi,
		xi_lag = 0
  )
	ct <- getC(Century)
}

test.timedependentInput<-function(){
	C0 <- c(1, 2, 15, 8, 3)
	time <- seq(0, 1, 1/12)
	temp <- c(0, -7, 2, 5, 11, 12, 16, 13, 13, 8, 4, 3, 2)
	rainfall <- c(63, 12, 33, 22, 24, 80, 52, 133, 57, 76, 55, 92, 71)
	pet <- c(35, 16, 48, 59, 115, 110, 139, 114, 103, 71, 47, 46, 42)
	xi <- fT.Century1(temp) * fW.Century(rainfall, pet)
	xi <- data.frame(time, xi=xi[1:length(time)])
  ut <- data.frame(time, rep(10, length.out=length(time)))
	
	Century <- CenturyModel(
    t = time,
		C0 = C0,
		LN = 0.5,
		Ls = 0.1,
		In = ut,
		clay = 0.1,
		silt = 0.2,
		xi = xi,
		xi_lag = 0
  )
	ct <- getC(Century)
}
