pkgname <- "SoilR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "SoilR-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('SoilR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("AWBmodel")
### * AWBmodel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: AWBmodel
### Title: Implementation of the microbial model AWB (Allison, Wallenstein,
###   Bradford, 2010)
### Aliases: AWBmodel

### ** Examples

hours=seq(0,800,0.1)

#Run the model with default parameter values
bcmodel=AWBmodel(t=hours)
Cpools=getC(bcmodel)
##Time solution
# fixme mm:
# the next line causes trouble on Rforge Windows patched build
# matplot(hours,Cpools,type="l",ylab="Concentrations",xlab="Hours",lty=1,ylim=c(0,max(Cpools)*1.2))
##State-space diagram
plot(as.data.frame(Cpools))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("AWBmodel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("C14Atm")
### * C14Atm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: C14Atm
### Title: Atmospheric 14C fraction
### Aliases: C14Atm
### Keywords: datasets

### ** Examples

#Notice that C14Atm is a shorter version of C14Atm_NH
require("SoilR")
data("C14Atm_NH")
plot(C14Atm_NH,type="l")
lines(C14Atm,col=2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("C14Atm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("C14Atm_NH")
### * C14Atm_NH

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: C14Atm_NH
### Title: Post-bomb atmospheric 14C fraction
### Aliases: C14Atm_NH
### Keywords: datasets

### ** Examples

plot(C14Atm_NH,type="l")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("C14Atm_NH", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CenturyModel")
### * CenturyModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CenturyModel
### Title: Implementation of the Century model
### Aliases: CenturyModel

### ** Examples

mnths=seq(0,100)
APPT=50 # Assume 50 cm annual precipitation
Pmax=-40+7.7*APPT # Max aboveground production
Rmax=100+7.0*APPT # Max belowground production
abvgIn=Pmax/(Pmax+Rmax)
blgIn=Rmax/(Pmax+Rmax)

cm=CenturyModel(t=mnths, surfaceIn = abvgIn, soilIn = blgIn, LN=0.5, Ls=0.1)
Ct=getC(cm)

poolNames=c("Surface structural", "Surface metabolic", "Belowground structural",
               "Belowground metabolic", "Active SOM", "Slow SOM", "Passive SOM")
matplot(mnths,Ct, type="l", lty=1, col=1:7, xlab="Time (months)", ylab="Carbon stock ")
legend("topleft", poolNames, lty=1, col=1:7, bty="n")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CenturyModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CenturyModel14")
### * CenturyModel14

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CenturyModel14
### Title: Implementation of a radiocarbon version of the Century model
### Aliases: CenturyModel14

### ** Examples

cal_yrs=seq(1900,2015, by=1/12)
APPT=50 # Assume 50 cm annual precipitation
Pmax=-40+7.7*APPT # Max aboveground production
Rmax=100+7.0*APPT # Max belowground production
abvgIn=52*Pmax/(Pmax+Rmax)
blgIn=52*Rmax/(Pmax+Rmax)
AtmC14=Graven2017[,c("Year.AD", "NH")]

cm=CenturyModel14(t=cal_yrs, surfaceIn = abvgIn, soilIn = blgIn, 
                  F0_Delta14C=rep(0,7), inputFc=AtmC14, LN=0.5, Ls=0.1)
C14t=getF14(cm)

poolNames=c("Surface structural", "Surface metabolic", "Belowground structural",
               "Belowground metabolic", "Active SOM", "Slow SOM", "Passive SOM")
plot(AtmC14, type="l", ylab="Delta 14C (per mil)")
matlines(cal_yrs,C14t, lty=1, col=2:8)
legend("topleft", poolNames, lty=1, col=2:8, bty="n")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CenturyModel14", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("GaudinskiModel14")
### * GaudinskiModel14

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: GaudinskiModel14
### Title: Implementation of a the six-pool C14 model proposed by Gaudinski
###   et al. 2000
### Aliases: GaudinskiModel14

### ** Examples

years=seq(1901,2010,by=0.5)

Ex=GaudinskiModel14(
t=years,
ks=c(kr=1/3, koi=1/1.5, koeal=1/4, koeah=1/80, kA1=1/3, kA2=1/75, kM=1/110),
inputFc=C14Atm_NH
)
R14m=getF14R(Ex)
C14m=getF14C(Ex)

plot(
C14Atm_NH,
type="l",
xlab="Year",
ylab=expression(paste(Delta^14,"C (per mil)")),
xlim=c(1940,2010)
) 
lines(years,C14m,col=4)
points(HarvardForest14CO2[1:11,1],HarvardForest14CO2[1:11,2],pch=19,cex=0.5)
points(HarvardForest14CO2[12:173,1],HarvardForest14CO2[12:173,2],pch=19,col=2,cex=0.5)
points(HarvardForest14CO2[158,1],HarvardForest14CO2[158,2],pch=19,cex=0.5)
lines(years,R14m,col=2)
legend(
"topright",
c("Delta 14C Atmosphere",
"Delta 14C SOM", 
"Delta 14C Respired"
),
lty=c(1,1,1), 
col=c(1,4,2),
bty="n"
)
## We now show how to bypass soilR s parameter sanity check if nacessary 
## (e.g in for parameter estimation ) in functions
## which might call it with unreasonable parameters
years=seq(1800,2010,by=0.5)
Ex=GaudinskiModel14(
t=years,
ks=c(kr=1/3,koi=1/1.5,koeal=1/4,koeah=1/80,kA1=1/3,kA2=1/75,kM=1/110),
inputFc=C14Atm_NH,
pass=TRUE
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("GaudinskiModel14", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("GeneralNlModel")
### * GeneralNlModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: GeneralNlModel
### Title: Use this function to create objects of class NlModel.
### Aliases: GeneralNlModel

### ** Examples

t_start=0
t_end=20
tn=100
timestep=(t_end-t_start)/tn
t=seq(t_start,t_end,timestep)
k1=1/2
k2=1/3
Km=0.5
nr=2

alpha=list()
alpha[["1_to_2"]]=function(C,t){
1/5
}
alpha[["2_to_1"]]=function(C,t){
1/6
}

f=function(C,t){
# The only thing to take care of is that we release a vector of the same
# size as C
S=C[[1]]
M=C[[2]]
O=matrix(byrow=TRUE,nrow=2,c(k1*M*(S/(Km+S)),
k2*M))
return(O)
}
Anl=new("TransportDecompositionOperator",t_start,Inf,nr,alpha,f)


c01=3
c02=2
iv=c(c01,c02)
inputrates=new("TimeMap",t_start,t_end,function(t){return(matrix(
nrow=nr,
ncol=1,
c( 2,  2)
))})
#################################################################################
# we check if we can reproduce the linear decomposition operator from the
# nonlinear one



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("GeneralNlModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Graven2017")
### * Graven2017

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Graven2017
### Title: Compiled records of radicarbon in atmospheric CO2 for historical
###   simulations in CMIP6
### Aliases: Graven2017
### Keywords: datasets

### ** Examples

matplot(Graven2017[,1], Graven2017[,-1],type="l",
       lty=1, xlab="Year AD", ylab="Delta14C (per mil)", bty="n")
legend("topleft",names(Graven2017[,-1]), lty=1, col=1:3, bty="n")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Graven2017", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("HarvardForest14CO2")
### * HarvardForest14CO2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: HarvardForest14CO2
### Title: Delta14C in soil CO2 efflux from Harvard Forest
### Aliases: HarvardForest14CO2
### Keywords: datasets

### ** Examples

plot(HarvardForest14CO2[,1:2])




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("HarvardForest14CO2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Hua2013")
### * Hua2013

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Hua2013
### Title: Atmospheric radiocarbon for the period 1950-2010 from Hua et al.
###   (2013)
### Aliases: Hua2013
### Keywords: datasets

### ** Examples

plot(Hua2013$NHZone1$Year.AD, Hua2013$NHZone1$mean.Delta14C, 
     type="l",xlab="Year AD",ylab=expression(paste(Delta^14,"C (\u2030)")))
lines(Hua2013$NHZone2$Year.AD,Hua2013$NHZone2$mean.Delta14C,col=2)
lines(Hua2013$NHZone3$Year.AD,Hua2013$NHZone3$mean.Delta14C,col=3)
lines(Hua2013$SHZone12$Year.AD,Hua2013$SHZone12$mean.Delta14C,col=4)
lines(Hua2013$SHZone3$Year.AD,Hua2013$SHZone3$mean.Delta14C,col=5)
legend(
	"topright",
	c(
		"Norther hemisphere zone 1",
		"Norther hemisphere zone 2",
		"Norther hemisphere zone 3",
                "Southern hemisphere zones 1 and 2",
		"Southern Hemispher zone 3"
	),
	lty=1,
	col=1:5,
	bty="n"
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Hua2013", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Hua2021")
### * Hua2021

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Hua2021
### Title: Atmospheric radiocarbon for the period 1950-2019 from Hua et al.
###   (2021)
### Aliases: Hua2021
### Keywords: datasets

### ** Examples

plot(Hua2021$NHZone1[,1:2],type="l")
lines(Hua2021$NHZone2[,1:2],col=2)
lines(Hua2021$NHZone3[,1:2],col=3)
lines(Hua2021$`SHZone1-2`[,1:2],col=4)
lines(Hua2021$SHZone3[,1:2],col=5)
legend("topright",names(Hua2021), col=1:5,lty=1,bty="n")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Hua2021", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ICBMModel")
### * ICBMModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ICBMModel
### Title: Implementation of the Introductory Carbon Balance Model (ICBM)
### Aliases: ICBMModel

### ** Examples

# examples from external files
# inst/examples/exICBMModel.R exICBMModel_paper:

    # This example reproduces the simulations 
    # presented in Table 1 of Andren and Katterer (1997).
    # First, the model is run for different values of the 
    # parameters representing different field experiments. 
    times=seq(0,20,by=0.1)
    Bare=ICBMModel(t=times) #Bare fallow
    pNpS=ICBMModel(t=times, h=0.125, r=1,    c0=c(0.3,4.11),  In=0.19+0.095) #+N +Straw
    mNpS=ICBMModel(t=times, h=0.125, r=1.22, c0=c(0.3, 4.05), In=0.19+0.058) #-N +Straw
    mNmS=ICBMModel(t=times, h=0.125, r=1.17, c0=c(0.3, 3.99), In=0.057) #-N -Straw
    pNmS=ICBMModel(t=times, h=0.125, r=1.07, c0=c(0.3, 4.02), In=0.091) #+N -Straw
    FM=ICBMModel(t=times, h=0.250, r=1.10, c0=c(0.3, 3.99), In=0.19+0.082) #Manure
    SwS=ICBMModel(t=times, h=0.340, r=0.97, c0=c(0.3, 4.14), In=0.19+0.106) #Sewage Sludge
    SS=ICBMModel(t=times, h=0.125, r=1.00, c0=c(0.25, 4.16), In=0.2)  #Steady State

    #The amount of carbon for each simulation is recovered with the function getC
    CtBare=getC(Bare)
    CtpNpS=getC(pNpS)
    CtmNpS=getC(mNpS)
    CtmNmS=getC(mNmS)
    CtpNmS=getC(pNmS)
    CtFM=getC(FM)
    CtSwS=getC(SwS)
    CtSS=getC(SS)

    #This plot reproduces Figure 1 in Andren and Katterer (1997)
    plot(times,
      rowSums(CtBare),
      type="l",
      ylim=c(0,8),
      xlim=c(0,20),
      ylab="Topsoil carbon mass (kg m-2)",
      xlab="Time (years)"
    )
    lines(times,rowSums(CtpNpS),lty=2)
    lines(times,rowSums(CtmNpS),lty=3)
    lines(times,rowSums(CtmNmS),lty=4)
    lines(times,rowSums(CtpNmS),lwd=2)
    lines(times,rowSums(CtFM),lty=2,lwd=2)
    lines(times,rowSums(CtSwS),lty=3,lwd=2)
    #lines(times,rowSums(CtSS),lty=4,lwd=2)
    legend("topleft",
      c("Bare fallow",
        "+N +Straw",
        "-N +Straw",
        "-N -Straw",
        "+N -Straw",
        "Manure",
       "Sludge"
      ),
      lty=c(1,2,3,4,1,2,3),
      lwd=c(1,1,1,1,2,2,2),
      bty="n"
    )




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ICBMModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("IntCal09")
### * IntCal09

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: IntCal09
### Title: Northern Hemisphere atmospheric radiocarbon for the pre-bomb
###   period
### Aliases: IntCal09
### Keywords: datasets

### ** Examples

par(mfrow=c(2,1))
plot(IntCal09$CAL.BP, IntCal09$C14.age, type="l")
polygon(x=c(IntCal09$CAL.BP,rev(IntCal09$CAL.BP)),
	y=c(IntCal09$C14.age+IntCal09$Error,rev(IntCal09$C14.age-IntCal09$Error)),
	col="gray",border=NA)
lines(IntCal09$CAL.BP,IntCal09$C14.age)

plot(IntCal09$CAL.BP,IntCal09$Delta.14C,type="l")
polygon(x=c(IntCal09$CAL.BP,rev(IntCal09$CAL.BP)),
	y=c(IntCal09$Delta.14C+IntCal09$Sigma,rev(IntCal09$Delta.14C-IntCal09$Sigma)),
	col="gray",border=NA)
lines(IntCal09$CAL.BP,IntCal09$Delta.14C)
par(mfrow=c(1,1))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("IntCal09", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("IntCal13")
### * IntCal13

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: IntCal13
### Title: Atmospheric radiocarbon for the 0-50,000 yr BP period
### Aliases: IntCal13
### Keywords: datasets

### ** Examples

     plot(IntCal13$CAL.BP,IntCal13$C14.age-IntCal13$Error,type="l",col=2,
          xlab="cal BP",ylab="14C BP")
     lines(IntCal13$CAL.BP,IntCal13$C14.age+IntCal13$Error,col=2)

     plot(IntCal13$CAL.BP,IntCal13$Delta.14C+IntCal13$Sigma,type="l",col=2,
          xlab="cal BP",ylab="Delta14C")
     lines(IntCal13$CAL.BP,IntCal13$Delta.14C-IntCal13$Sigma,col=2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("IntCal13", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("IntCal20")
### * IntCal20

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: IntCal20
### Title: The IntCal20 northern hemisphere radiocarbon curve for the
###   0-55,000 yr BP period
### Aliases: IntCal20
### Keywords: datasets

### ** Examples

    plot(IntCal20$CAL.BP, IntCal20$Delta.14C, type="l", 
         xlab="cal BP", ylab="Delta14C (per mil)")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("IntCal20", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Model")
### * Model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Model
### Title: Constructor for class Model
### Aliases: Model

### ** Examples


# vim:set ff=unix expandtab ts=2 sw=2:
test.all.possible.Model.arguments <- function(){
  # This example shows different kinds of arguments to the function Model.
  # The model objects we will build will share some common features.
  #  - two pools 
  #  - initial values 

  iv<-  c(5,6)
  times <- seq(1,10,by=0.1)

  # The other parameters A and inputFluxes will be different
  # The function Model will transform these arguments 
  # into objects of the classes required by the internal constructor.
  # This leads to a number of possible argument types. 
  # We demonstrate some of the possibilities here.
  # Let us first look at the choeices for argument 'A'.
  
  #) 
  possibleAs  <- example.2DGeneralDecompOpArgs()
  
  # Since "Model" will call "InFluxes" on its "inputFluxes" 
  # argument there are again different choices
  # we have included a function in SoilR that produces 2D examples
  
  possibleInfluxes <- example.2DInFluxes.Args()
 print(possibleInfluxes$I.vec)
  # We can build a lot of  models from the possible combinations
  # for instance   
  #m1 <- Model(
  #        t=times,
  #        A=matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2)),
  #        ivList=iv,
  #        inputFluxes=possibleInfluxes$I.vec) 
  ## We now produce all combinations of As and InputFluxes
  combinations <- listProduct(possibleAs,possibleInfluxes)
  print(length(combinations))
  # and a Model for each
  models <- lapply(
              combinations,
              function(combi){
                #Model(t=times,A=combi$A,ivList=iv,inputFluxes=combi$I)
                Model(t=times,A=combi[[1]],ivList=iv,inputFluxes=combi[[2]])
              }
            )
  ## lets check that we can compute something# 
  lapply(models,getC)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Model_14")
### * Model_14

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Model_14
### Title: general constructor for class Model_14
### Aliases: Model_14

### ** Examples

# examples from external files
# inst/tests/requireSoilR/runit.all.possible.Model.arguments.R test.all.possible.Model.arguments:

  # This example shows different kinds of arguments to the function Model.
  # The model objects we will build will share some common features.
  #  - two pools 
  #  - initial values 

       iv<-  c(5,6)

  #  - times 

       times <- seq(1,10,by=0.1)

  # The other parameters A and inputFluxes will be different
  # The function Model will transform these arguments 
  # into objects of the classes required by the internal constructor.
  # This leads to a number of possible argument types. 
  # We demonstrate some of the possibilities here.
  # Let us first look at the choeices for argument 'A'.
  
  #) 
  possibleAs  <- example.2DGeneralDecompOpArgs()
  
  # Since "Model" will call "InFluxes" on its "inputFluxes" 
  # argument there are again different choices
  # we have included a function in SoilR that produces 2D examples
  
  possibleInfluxes <- example.2DInFluxes.Args()
 print(possibleInfluxes$I.vec)
  # We can build a lot of  models from the possible combinations
  # for instance   
  #m1 <- Model(
  #        t=times,
  #        A=matrix(nrow=2,byrow=TRUE,c(-0.1,0,0,-0.2)),
  #        ivList=iv,
  #        inputFluxes=possibleInfluxes$I.vec) 
  ## We now produce that all combinations of As and InputFluxes
  combinations <- listProduct(possibleAs,possibleInfluxes)
  print(length(combinations))
  # an a Model for each
  models <- lapply(
              combinations,
              function(combi){
                #Model(t=times,A=combi$A,ivList=iv,inputFluxes=combi$I)
                Model(t=times,A=combi[[1]],ivList=iv,inputFluxes=combi[[2]])
              }
            )
  ## lets check that we can compute something# 
  lapply(models,getC)

# inst/examples/ModelExamples.R CorrectNonautonomousLinearModelExplicit:

  # This example describes the creation and use of a Model object that 
  # is defined by time dependent functions for decomposition and influx.
  # The constructor of the Model-class  (see  ?Model) 
  # works for different combinations of 
  # arguments.
  # Although Model (the constructor function for objects of this class 
  # accepts many many more convenient kinds of arguments,
  # we will in this example call the constructor whith arguments which 
  # are of the same type as one of hte current internal 
  # representations in the 
  # Model object and create these arguments explicitly beforehand 
  # to demonstrate the approach with the most flexibility.
  # We start with the Decomposition Operator.
  # For this example we assume that we are able to describe the
  # decomposition ofperator  by explicit R functions that are valid 
  # for a finite time interval.
  # Therefore we choose the appropriate  sub class BoundLinDecompOp
  # of DecompOp explicitly.  (see ?'BoundLinDecompOp-class') 
  A=BoundLinDecompOp(
    ## We call the generic constructor (see ?BoundLindDcompOp) 
    ## which has a method  
    ## that takes a matrix-valued function of time as its first argument.
    ## (Although Model accepts time series data directly and 
    ## will derive the internally used interpolating for you, 
    ## the function argument could for instance represent the result
    ## of a very sophisticated interpolation performed by yourself)
    function(t){
      matrix(nrow=3,ncol=3,byrow=TRUE,
         c(
           -1,    0,        0,
          0.5,   -2,        0,
            0,    1, sin(t)-1 
        )
      )    
    },
    ## The other two arguments describe the time interval where the 
    ## function is valid (the domain of the function)
    ## The interval will be checked against the domain of the InFlux
    ## argument of Model and against its 't' argument to avoid 
    ## invalid computations outside the domain. 
    ## (Inf and -Inf are possible values, but should only be used 
    ## if the function is really valid for all times, which is 
    ## especially untrue for functions resulting from interpolations,
    ## which are usually extremely misleading for arguments outside the 
    ## domain covered by the data that has been used for the interpolation.)
    ## This is a safety net against wrong results origination from unitendet EXTRApolation )
    starttime=0,
    endtime=20
  )  
  I=BoundInFluxes(
     ## The first argument is a vector-valued function of time
     function(t){
       matrix(nrow=3,ncol=1,byrow=TRUE,
           c(-1,    0,    0)
       )
     },
     ## The other two arguments describe the time interval where the 
     ## function is valid (the domain of the function)
     starttime=0,
     endtime=40
  )
  ## No we specify the points in time where we want 
  ## to compute results
  t_start=0 
  t_end=10 
  tn=50
  timestep <- (t_end-t_start)/tn 
  times <- seq(t_start,t_end,timestep) 
  ## and the start values
  sv=c(0,0,0)
  mod=Model(t=times,A,sv,I)

  ## No we use the model to compute some results
  getC(mod)
  getReleaseFlux(mod)
  #also look at the methods section of Model-class 




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Model_14", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("OnepModel")
### * OnepModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: OnepModel
### Title: Implementation of a one pool model
### Aliases: OnepModel

### ** Examples

t_start=0 
t_end=10 
tn=50
timestep=(t_end-t_start)/tn 
t=seq(t_start,t_end,timestep) 
k=0.8
C0=100
In = 30


Ex=OnepModel(t,k,C0,In)
Ct=getC(Ex)
Rt=getReleaseFlux(Ex)
Rc=getAccumulatedRelease(Ex)

plot(
t,
Ct,
type="l",
ylab="Carbon stocks (arbitrary units)",
xlab="Time (arbitrary units)",
lwd=2
) 

plot(
t,
Rt,
type="l",
ylab="Carbon released (arbitrary units)",
xlab="Time (arbitrary units)",
lwd=2
) 

plot(
t,
Rc,
type="l",
ylab="Cummulative carbon released (arbitrary units)",
xlab="Time (arbitrary units)",
lwd=2
) 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("OnepModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("OnepModel14")
### * OnepModel14

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: OnepModel14
### Title: Implementation of a one-pool C14 model
### Aliases: OnepModel14

### ** Examples

years=seq(1901,2009,by=0.5)
LitterInput=700 

Ex=OnepModel14(t=years,k=1/10,C0=500, F0=0,In=LitterInput, inputFc=C14Atm_NH)
C14t=getF14(Ex)

plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years, C14t[,1], col=4)
legend(
"topright",
c("Delta 14C Atmosphere", "Delta 14C in SOM"),
lty=c(1,1),
col=c(1,4),
lwd=c(1,1),
bty="n"
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("OnepModel14", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ParallelModel")
### * ParallelModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ParallelModel
### Title: models for unconnected pools
### Aliases: ParallelModel

### ** Examples

t_start=0 
t_end=10 
tn=50
timestep=(t_end-t_start)/tn 
t=seq(t_start,t_end,timestep) 
k=TimeMap(
function(times){c(-0.5,-0.2,-0.3)},
t_start,
t_end
)
c0=c(1, 2, 3)
#constant inputrates
inputrates=BoundInFluxes(
function(t){matrix(nrow=3,ncol=1,c(1,1,1))},
t_start,
t_end
) 
mod=ParallelModel(t,k,c0,inputrates)
Y=getC(mod)
lt1=1 ;lt2=2 ;lt3=3 
col1=1; col2=2; col3=3
plot(t,Y[,1],type="l",lty=lt1,col=col1,
ylab="C stocks",xlab="Time") 
lines(t,Y[,2],type="l",lty=lt2,col=col2) 
lines(t,Y[,3],type="l",lty=lt3,col=col3) 
legend(
"topleft",
c("C in pool 1",
"C in 2",
"C in pool 3"
),
lty=c(lt1,lt2,lt3),
col=c(col1,col2,col3)
)
Y=getAccumulatedRelease(mod)
plot(t,Y[,1],type="l",lty=lt1,col=col1,ylab="C release",xlab="Time") 
lines(t,Y[,2],lt2,type="l",lty=lt2,col=col2) 
lines(t,Y[,3],type="l",lty=lt3,col=col3) 
legend("topright",c("R1","R2","R3"),lty=c(lt1,lt2,lt3),col=c(col1,col2,col3))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ParallelModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RothCModel")
### * RothCModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RothCModel
### Title: Implementation of the RothCModel
### Aliases: RothCModel

### ** Examples

t=0:500 
Ex=RothCModel(t)
Ct=getC(Ex)
Rt=getReleaseFlux(Ex)

matplot(t,Ct,type="l",col=1:5, ylim=c(0,25),
ylab=expression(paste("Carbon stores (Mg C ", ha^-1,")")),
xlab="Time (years)", lty=1)
lines(t,rowSums(Ct),lwd=2)
legend("topleft",
c("Pool 1, DPM",
"Pool 2, RPM",
"Pool 3, BIO",
"Pool 4, HUM",
"Pool 5, IOM",
"Total Carbon"),
lty=1,
lwd=c(rep(1,5),2),
col=c(1:5,1),
bty="n"
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RothCModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SHCal20")
### * SHCal20

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SHCal20
### Title: The SHCal20 southern hemisphere radiocarbon curve for the
###   0-55,000 yr BP period
### Aliases: SHCal20
### Keywords: datasets

### ** Examples

    plot(SHCal20$CAL.BP, SHCal20$Delta.14C, type="l", 
         xlab="cal BP", ylab="Delta14C (per mil)")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SHCal20", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SeriesLinearModel")
### * SeriesLinearModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SeriesLinearModel
### Title: General m-pool linear model with series structure
### Aliases: SeriesLinearModel

### ** Examples

#A five-pool model
t_start=0 
t_end=10 
tn=50
timestep=(t_end-t_start)/tn 
t=seq(t_start,t_end,timestep) 
ks=c(k1=0.8,k2=0.4,k3=0.2, k4=0.1,k5=0.05)
Ts=c(0.5,0.2,0.2,0.1)
C0=c(C10=100,C20=150, C30=50, C40=50, C50=10)
In = 50
#
Ex1=SeriesLinearModel(t=t,m.pools=5,ki=ks,Tij=Ts,C0=C0,In=In,xi=fT.Q10(15))
Ct=getC(Ex1)
#
matplot(t,Ct,type="l",col=2:6,lty=1,ylim=c(0,sum(C0)))
lines(t,rowSums(Ct),lwd=2)
legend("topright",c("Total C","C in pool 1", "C in pool 2","C in pool 3",
"C in pool 4","C in pool 5"),
lty=1,col=1:6,lwd=c(2,rep(1,5)),bty="n")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SeriesLinearModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SeriesLinearModel14")
### * SeriesLinearModel14

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SeriesLinearModel14
### Title: General m-pool linear C14 model with series structure
### Aliases: SeriesLinearModel14

### ** Examples

years=seq(1901,2009,by=0.5)
LitterInput=700 

Ex=SeriesLinearModel14(
t=years,ki=c(k1=1/2.8, k2=1/35, k3=1/100), m.pools=3,
C0=c(200,5000,500), F0_Delta14C=c(0,0,0),
In=LitterInput, Tij=c(0.5, 0.1),inputFc=C14Atm_NH
)
R14m=getF14R(Ex)
C14m=getF14C(Ex)
C14t=getF14(Ex)

par(mfrow=c(2,1))
plot(C14Atm_NH,type="l",xlab="Year",
ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years, C14t[,1], col=4)
lines(years, C14t[,2],col=4,lwd=2)
lines(years, C14t[,3],col=4,lwd=3)
legend(
"topright",
c("Delta 14C Atmosphere", "Delta 14C pool 1", "Delta 14C pool 2", "Delta 14C pool 3"),
lty=rep(1,4),col=c(1,4,4,4),lwd=c(1,1,2,3),bty="n")

plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years,C14m,col=4)
lines(years,R14m,col=2)
legend("topright",c("Delta 14C Atmosphere","Delta 14C SOM", "Delta 14C Respired"),
lty=c(1,1,1), col=c(1,4,2),bty="n")
par(mfrow=c(1,1))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SeriesLinearModel14", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("ThreepFeedbackModel")
### * ThreepFeedbackModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ThreepFeedbackModel
### Title: Implementation of a three pool model with feedback structure
### Aliases: ThreepFeedbackModel

### ** Examples

t_start=0 
t_end=10 
tn=50
timestep=(t_end-t_start)/tn 
t=seq(t_start,t_end,timestep) 
ks=c(k1=0.8,k2=0.4,k3=0.2)
C0=c(C10=100,C20=150, C30=50)
In = 60

Temp=rnorm(t,15,1)
TempEffect=data.frame(t,fT.Daycent1(Temp))

Ex1=ThreepFeedbackModel(t=t,ks=ks,a21=0.5,a12=0.1,a32=0.2,a23=0.1,C0=C0,In=In,xi=TempEffect)
Ct=getC(Ex1)
Rt=getReleaseFlux(Ex1)

plot(
t,
rowSums(Ct),
type="l",
ylab="Carbon stocks (arbitrary units)",
xlab="Time (arbitrary units)",
lwd=2,
ylim=c(0,sum(Ct[51,]))
) 
lines(t,Ct[,1],col=2)
lines(t,Ct[,2],col=4)
lines(t,Ct[,3],col=3)
legend(
"topleft",
c("Total C","C in pool 1", "C in pool 2","C in pool 3"),
lty=c(1,1,1,1),
col=c(1,2,4,3),
lwd=c(2,1,1,1),
bty="n"
)

plot(
t,
rowSums(Rt),
type="l",
ylab="Carbon released (arbitrary units)",
xlab="Time (arbitrary units)",
lwd=2,
ylim=c(0,sum(Rt[51,]))
) 
lines(t,Rt[,1],col=2)
lines(t,Rt[,2],col=4)
lines(t,Rt[,3],col=3)
legend(
"topleft",
c("Total C release",
"C release from pool 1",
"C release from pool 2",
"C release from pool 3"),
lty=c(1,1,1,1),
col=c(1,2,4,3),
lwd=c(2,1,1,1),
bty="n"
)

Inr=data.frame(t,Random.inputs=rnorm(length(t),50,10))
plot(Inr,type="l")

Ex2=ThreepFeedbackModel(t=t,ks=ks,a21=0.5,a12=0.1,a32=0.2,a23=0.1,C0=C0,In=Inr)
Ctr=getC(Ex2)
Rtr=getReleaseFlux(Ex2)

plot(
t,
rowSums(Ctr),
type="l",
ylab="Carbon stocks (arbitrary units)",
xlab="Time (arbitrary units)",
lwd=2,
ylim=c(0,sum(Ctr[51,]))
) 
lines(t,Ctr[,1],col=2)
lines(t,Ctr[,2],col=4)
lines(t,Ctr[,3],col=3)
legend("topright",c("Total C","C in pool 1", "C in pool 2","C in pool 3"),
lty=c(1,1,1,1),col=c(1,2,4,3),lwd=c(2,1,1,1),bty="n")

plot(t,rowSums(Rtr),type="l",ylab="Carbon released (arbitrary units)",
xlab="Time (arbitrary units)",lwd=2,ylim=c(0,sum(Rtr[51,]))) 
lines(t,Rtr[,1],col=2)
lines(t,Rtr[,2],col=4)
lines(t,Rtr[,3],col=3)
legend(
"topright",
c("Total C release",
"C release from pool 1",
"C release from pool 2",
"C release from pool 3"
),
lty=c(1,1,1,1),
col=c(1,2,4,3),
lwd=c(2,1,1,1),
bty="n")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ThreepFeedbackModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ThreepFeedbackModel14")
### * ThreepFeedbackModel14

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ThreepFeedbackModel14
### Title: Implementation of a three-pool C14 model with feedback structure
### Aliases: ThreepFeedbackModel14

### ** Examples

#years=seq(1901,2009,by=0.5)
years=seq(1904,2009,by=0.5)
LitterInput=100
k1=1/2; k2=1/10; k3=1/50
a21=0.9*k1
a12=0.4*k2
a32=0.4*k2
a23=0.7*k3

Feedback=ThreepFeedbackModel14(
t=years,
ks=c(k1=k1, k2=k2, k3=k3),
C0=c(100,500,1000),
F0_Delta14C=c(0,0,0),
In=LitterInput,
a21=a21,
a12=a12,
a32=a32,
a23=a23,
inputFc=C14Atm_NH
)
F.R14m=getF14R(Feedback)
F.C14m=getF14C(Feedback)
F.C14t=getF14(Feedback)

Series=ThreepSeriesModel14(
t=years,
ks=c(k1=k1, k2=k2, k3=k3),
C0=c(100,500,1000),
F0_Delta14C=c(0,0,0),
In=LitterInput,
a21=a21,
a32=a32,
inputFc=C14Atm_NH
)
S.R14m=getF14R(Series)
S.C14m=getF14C(Series)
S.C14t=getF14(Series)

Parallel=ThreepParallelModel14(
t=years,
ks=c(k1=k1, k2=k2, k3=k3),
C0=c(100,500,1000),
F0_Delta14C=c(0,0,0),
In=LitterInput,
gam1=0.6,
gam2=0.2,
inputFc=C14Atm_NH,
lag=2
)
P.R14m=getF14R(Parallel)
P.C14m=getF14C(Parallel)
P.C14t=getF14(Parallel)

par(mfrow=c(3,2))
plot(
C14Atm_NH,
type="l",
xlab="Year",
ylab=expression(paste(Delta^14,"C ","(\u2030)")),
xlim=c(1940,2010)
) 
lines(years, P.C14t[,1], col=4)
lines(years, P.C14t[,2],col=4,lwd=2)
lines(years, P.C14t[,3],col=4,lwd=3)
legend(
"topright",
c("Atmosphere", "Pool 1", "Pool 2", "Pool 3"),
lty=rep(1,4),
col=c(1,4,4,4),
lwd=c(1,1,2,3),
bty="n"
)

plot(C14Atm_NH,type="l",xlab="Year",
ylab=expression(paste(Delta^14,"C ","(\u2030)")),xlim=c(1940,2010)) 
lines(years,P.C14m,col=4)
lines(years,P.R14m,col=2)
legend("topright",c("Atmosphere","Bulk SOM", "Respired C"),
lty=c(1,1,1), col=c(1,4,2),bty="n")

plot(C14Atm_NH,type="l",xlab="Year",
ylab=expression(paste(Delta^14,"C ","(\u2030)")),xlim=c(1940,2010)) 
lines(years, S.C14t[,1], col=4)
lines(years, S.C14t[,2],col=4,lwd=2)
lines(years, S.C14t[,3],col=4,lwd=3)
legend("topright",c("Atmosphere", "Pool 1", "Pool 2", "Pool 3"),
lty=rep(1,4),col=c(1,4,4,4),lwd=c(1,1,2,3),bty="n")

plot(C14Atm_NH,type="l",xlab="Year",
ylab=expression(paste(Delta^14,"C ","(\u2030)")),xlim=c(1940,2010)) 
lines(years,S.C14m,col=4)
lines(years,S.R14m,col=2)
legend("topright",c("Atmosphere","Bulk SOM", "Respired C"),
lty=c(1,1,1), col=c(1,4,2),bty="n")

plot(C14Atm_NH,type="l",xlab="Year",
ylab=expression(paste(Delta^14,"C ","(\u2030)")),xlim=c(1940,2010)) 
lines(years, F.C14t[,1], col=4)
lines(years, F.C14t[,2],col=4,lwd=2)
lines(years, F.C14t[,3],col=4,lwd=3)
legend("topright",c("Atmosphere", "Pool 1", "Pool 2", "Pool 3"),
lty=rep(1,4),col=c(1,4,4,4),lwd=c(1,1,2,3),bty="n")

plot(C14Atm_NH,type="l",xlab="Year",
ylab=expression(paste(Delta^14,"C ","(\u2030)")),xlim=c(1940,2010)) 
lines(years,F.C14m,col=4)
lines(years,F.R14m,col=2)
legend("topright",c("Atmosphere","Bulk SOM", "Respired C"),
lty=c(1,1,1), col=c(1,4,2),bty="n")


par(mfrow=c(1,1))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ThreepFeedbackModel14", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("ThreepParallelModel")
### * ThreepParallelModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ThreepParallelModel
### Title: Implementation of a three pool model with parallel structure
### Aliases: ThreepParallelModel

### ** Examples

t_start=0 
t_end=10 
tn=50
timestep=(t_end-t_start)/tn 
t=seq(t_start,t_end,timestep) 

Ex=ThreepParallelModel(t,ks=c(k1=0.5,k2=0.2,k3=0.1),
C0=c(c10=100, c20=150,c30=50),In=20,gam1=0.7,gam2=0.1,xi=0.5)
Ct=getC(Ex)

plot(t,rowSums(Ct),type="l",lwd=2,
ylab="Carbon stocks (arbitrary units)",xlab="Time",ylim=c(0,sum(Ct[1,]))) 
lines(t,Ct[,1],col=2)
lines(t,Ct[,2],col=4)
lines(t,Ct[,3],col=3)
legend("topright",c("Total C","C in pool 1", "C in pool 2","C in pool 3"),
lty=c(1,1,1,1),col=c(1,2,4,3),lwd=c(2,1,1,1),bty="n")

Rt=getReleaseFlux(Ex)
plot(t,rowSums(Rt),type="l",ylab="Carbon released (arbitrary units)",
xlab="Time",lwd=2,ylim=c(0,sum(Rt[1,]))) 
lines(t,Rt[,1],col=2)
lines(t,Rt[,2],col=4)
lines(t,Rt[,3],col=3)
legend("topright",c("Total C release","C release from pool 1",
"C release from pool 2","C release from pool 3"),
lty=c(1,1,1,1),col=c(1,2,4,3),lwd=c(2,1,1,1),bty="n")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ThreepParallelModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ThreepParallelModel14")
### * ThreepParallelModel14

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ThreepParallelModel14
### Title: Implementation of a three-pool C14 model with parallel structure
### Aliases: ThreepParallelModel14

### ** Examples

years=seq(1903,2009,by=0.5) # note that we 
LitterInput=700 

Ex=ThreepParallelModel14(
t=years,
ks=c(k1=1/2.8, k2=1/35, k3=1/100),
C0=c(200,5000,500),
F0_Delta14C=c(0,0,0),
In=LitterInput,
gam1=0.7,
gam2=0.1,
inputFc=C14Atm_NH,
lag=2
)
R14m=getF14R(Ex)
C14m=getF14C(Ex)
C14t=getF14(Ex)

par(mfrow=c(2,1))
plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years, C14t[,1], col=4)
lines(years, C14t[,2],col=4,lwd=2)
lines(years, C14t[,3],col=4,lwd=3)
legend(
"topright",
c(
"Delta 14C Atmosphere", 
"Delta 14C pool 1",
"Delta 14C pool 2", 
"Delta 14C pool 3"
),
lty=rep(1,4),
col=c(1,4,4,4),
lwd=c(1,1,2,3),
bty="n"
)

plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years,C14m,col=4)
lines(years,R14m,col=2)
legend("topright",c("Delta 14C Atmosphere","Delta 14C SOM", "Delta 14C Respired"),
lty=c(1,1,1), col=c(1,4,2),bty="n")
par(mfrow=c(1,1))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ThreepParallelModel14", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("ThreepSeriesModel")
### * ThreepSeriesModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ThreepSeriesModel
### Title: Implementation of a three pool model with series structure
### Aliases: ThreepSeriesModel

### ** Examples

t_start=0 
t_end=10 
tn=50
timestep=(t_end-t_start)/tn 
t=seq(t_start,t_end,timestep) 
ks=c(k1=0.8,k2=0.4,k3=0.2)
C0=c(C10=100,C20=150, C30=50)
In = 50

Ex1=ThreepSeriesModel(t=t,ks=ks,a21=0.5,a32=0.2,C0=C0,In=In,xi=fT.Q10(15))
Ct=getC(Ex1)
Rt=getReleaseFlux(Ex1)

plot(t,rowSums(Ct),type="l",ylab="Carbon stocks (arbitrary units)",
xlab="Time (arbitrary units)",lwd=2,ylim=c(0,sum(Ct[1,]))) 
lines(t,Ct[,1],col=2)
lines(t,Ct[,2],col=4)
lines(t,Ct[,3],col=3)
legend("topright",c("Total C","C in pool 1", "C in pool 2","C in pool 3"),
lty=c(1,1,1,1),col=c(1,2,4,3),lwd=c(2,1,1,1),bty="n")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ThreepSeriesModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ThreepSeriesModel14")
### * ThreepSeriesModel14

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ThreepSeriesModel14
### Title: Implementation of a three-pool C14 model with series structure
### Aliases: ThreepSeriesModel14

### ** Examples

years=seq(1901,2009,by=0.5)
LitterInput=700 

Ex=ThreepSeriesModel14(
t=years,ks=c(k1=1/2.8, k2=1/35, k3=1/100),
C0=c(200,5000,500), F0_Delta14C=c(0,0,0),
In=LitterInput, a21=0.1, a32=0.01,inputFc=C14Atm_NH
)
R14m=getF14R(Ex)
C14m=getF14C(Ex)
C14t=getF14(Ex)

par(mfrow=c(2,1))
plot(C14Atm_NH,type="l",xlab="Year",
ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years, C14t[,1], col=4)
lines(years, C14t[,2],col=4,lwd=2)
lines(years, C14t[,3],col=4,lwd=3)
legend(
"topright",
c("Delta 14C Atmosphere", "Delta 14C pool 1", "Delta 14C pool 2", "Delta 14C pool 3"),
lty=rep(1,4),col=c(1,4,4,4),lwd=c(1,1,2,3),bty="n")

plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years,C14m,col=4)
lines(years,R14m,col=2)
legend("topright",c("Delta 14C Atmosphere","Delta 14C SOM", "Delta 14C Respired"),
lty=c(1,1,1), col=c(1,4,2),bty="n")
par(mfrow=c(1,1))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ThreepSeriesModel14", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("ThreepairMMmodel")
### * ThreepairMMmodel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ThreepairMMmodel
### Title: Implementation of a 6-pool Michaelis-Menten model
### Aliases: ThreepairMMmodel

### ** Examples

days=seq(0,1000)
#Run the model with default parameter values
MMmodel=ThreepairMMmodel(t=days,ival=rep(c(100,10),3),ks=c(0.1,0.05,0.01),
kb=c(0.005,0.001,0.0005),Km=c(100,150,200),r=c(0.9,0.9,0.9),
ADD=c(3,1,0.5))
Cpools=getC(MMmodel)
#Time solution
matplot(days,Cpools,type="l",ylab="Concentrations",xlab="Days",lty=rep(1:2,3),
ylim=c(0,max(Cpools)*1.2),col=rep(1:3,each=2),
main="Multi-substrate microbial model")
legend("topright",c("Substrate 1", "Microbial biomass 1", 
"Substrate 2", "Microbial biomass 2",
"Substrate 3", "Microbial biomass 3"),
lty=rep(1:2,3),col=rep(1:3,each=2),
bty="n")
#State-space diagram
plot(Cpools[,2],Cpools[,1],type="l",ylab="Substrate",xlab="Microbial biomass")
lines(Cpools[,4],Cpools[,3],col=2)
lines(Cpools[,6],Cpools[,5],col=3)
legend("topright",c("Substrate-Enzyme pair 1","Substrate-Enzyme pair 2",
"Substrate-Enzyme pair 3"),col=1:3,lty=1,bty="n")
#Microbial biomass over time
plot(days,Cpools[,2],type="l",col=2,xlab="Days",ylab="Microbial biomass")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ThreepairMMmodel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("TwopFeedbackModel")
### * TwopFeedbackModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TwopFeedbackModel
### Title: Implementation of a two pool model with feedback structure
### Aliases: TwopFeedbackModel

### ** Examples

#This example show the difference between the three types of two-pool models  
times=seq(0,20,by=0.1)
ks=c(k1=0.8,k2=0.00605)
C0=c(C10=5,C20=5)

Temp=rnorm(times,15,2)
WC=runif(times,10,20)
TempEffect=data.frame(times,fT=fT.Daycent1(Temp))
MoistEffect=data.frame(times, fW=fW.Daycent2(WC)[2])

Inmean=1
InRand=data.frame(times,Random.inputs=rnorm(length(times),Inmean,0.2))
InSin=data.frame(times,Inmean+0.5*sin(times*pi*2))

Parallel=TwopParallelModel(t=times,ks=ks,C0=C0,In=Inmean,gam=0.9,
xi=(fT.Daycent1(15)*fW.Demeter(15)))
Series=TwopSeriesModel(t=times,ks=ks,a21=0.2*ks[1],C0=C0,In=InSin,
xi=(fT.Daycent1(15)*fW.Demeter(15)))
Feedback=TwopFeedbackModel(t=times,ks=ks,a21=0.2*ks[1],a12=0.5*ks[2],C0=C0,
In=InRand,xi=MoistEffect)

CtP=getC(Parallel)
CtS=getC(Series)
CtF=getC(Feedback)

RtP=getReleaseFlux(Parallel)
RtS=getReleaseFlux(Series)
RtF=getReleaseFlux(Feedback)

par(mfrow=c(2,1),mar=c(4,4,1,1))
plot(times,rowSums(CtP),type="l",ylim=c(0,20),ylab="Carbon stocks (arbitrary units)",xlab=" ")
lines(times,rowSums(CtS),col=2)
lines(times,rowSums(CtF),col=3)
legend("topleft",c("Two-pool Parallel","Two-pool Series","Two-pool Feedback"),
lty=c(1,1,1),col=c(1,2,3),bty="n")

plot(times,rowSums(RtP),type="l",ylim=c(0,3),ylab="Carbon release (arbitrary units)", xlab="Time")
lines(times,rowSums(RtS),col=2)
lines(times,rowSums(RtF),col=3)
par(mfrow=c(1,1))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TwopFeedbackModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("TwopFeedbackModel14")
### * TwopFeedbackModel14

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TwopFeedbackModel14
### Title: Implementation of a two-pool C14 model with feedback structure
### Aliases: TwopFeedbackModel14

### ** Examples

years=seq(1901,2009,by=0.5)
LitterInput=700 

Ex=TwopFeedbackModel14(t=years,ks=c(k1=1/2.8, k2=1/35),C0=c(200,5000), 
F0_Delta14C=c(0,0),In=LitterInput, a21=0.1,a12=0.01,inputFc=C14Atm_NH)
R14m=getF14R(Ex)
C14m=getF14C(Ex)
C14t=getF14(Ex)

par(mfrow=c(2,1))
plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years, C14t[,1], col=4)
lines(years, C14t[,2],col=4,lwd=2)
legend("topright",c("Delta 14C Atmosphere", "Delta 14C pool 1", "Delta 14C pool 2"),
lty=c(1,1,1),col=c(1,4,4),lwd=c(1,1,2),bty="n")

plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years,C14m,col=4)
lines(years,R14m,col=2)
legend("topright",c("Delta 14C Atmosphere","Delta 14C SOM", "Delta 14C Respired"),
lty=c(1,1,1), col=c(1,4,2),bty="n")
par(mfrow=c(1,1))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TwopFeedbackModel14", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("TwopMMmodel")
### * TwopMMmodel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TwopMMmodel
### Title: Implementation of a two-pool Michaelis-Menten model
### Aliases: TwopMMmodel

### ** Examples

days=seq(0,1000,0.5)
MMmodel=TwopMMmodel(t=days,ival=c(100,10))
Cpools=getC(MMmodel)
matplot(days,Cpools,type="l",ylab="Concentrations",xlab="Days",lty=1,ylim=c(0,max(Cpools)*1.2))
legend("topleft",c("SOM-C", "Microbial biomass"),lty=1,col=c(1,2),bty="n")
ks=0.000018
kb=0.007
r=0.6
ADD=3.2
#Analytical solution of fixed points
#Cs_=kb/((1-r)*ks) wrong look at the sympy test print twopMModel.pdf
Km=900
Af=1
Cs=kb*Km/(Af*ks*(1-r)-kb)
abline(h=Cs,lty=2)
Cb=(ADD*(1-r))/(r*kb)
abline(h=Cb,lty=2,col=2)
#State-space diagram
plot(Cpools[,2],Cpools[,1],type="l",ylab="SOM-C",xlab="Microbial biomass")
plot(days,Cpools[,2],type="l",col=2,xlab="Days",ylab="Microbial biomass")

#The default parameterization exhaust the microbial biomass.
#A different behavior is obtained by increasing ks and decreasing kb
MMmodel=TwopMMmodel(t=days,ival=c(972,304) ,Af=3,kb=0.0000001)
Cpools=getC(MMmodel)

matplot(days,Cpools,type="l",ylab="Concentrations",xlab="Days",lty=1,ylim=c(0,max(Cpools)*1.2))
legend("topleft",c("SOM-C", "Microbial biomass"),lty=1,col=c(1,2),bty="n")

plot(Cpools[,2],Cpools[,1],type="l",ylab="SOM-C",xlab="Microbial biomass")

plot(days,Cpools[,2],type="l",col=2,xlab="Days",ylab="Microbial biomass")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TwopMMmodel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("TwopParallelModel")
### * TwopParallelModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TwopParallelModel
### Title: Implementation of a linear two pool model with parallel
###   structure
### Aliases: TwopParallelModel

### ** Examples

t_start=0 
t_end=10 
tn=50
timestep=(t_end-t_start)/tn 
t=seq(t_start,t_end,timestep) 
Ex=TwopParallelModel(t,ks=c(k1=0.5,k2=0.2),C0=c(c10=100, c20=150),In=10,gam=0.7,xi=0.5)
Ct=getC(Ex)
plot(t,rowSums(Ct),type="l",lwd=2,
ylab="Carbon stocks (arbitrary units)",xlab="Time",ylim=c(0,sum(Ct[1,]))) 
lines(t,Ct[,1],col=2)
lines(t,Ct[,2],col=4)
legend("topright",c("Total C","C in pool 1", "C in pool 2"),
lty=c(1,1,1),col=c(1,2,4),lwd=c(2,1,1),bty="n")

Rt=getReleaseFlux(Ex)
plot(t,rowSums(Rt),type="l",ylab="Carbon released (arbitrary units)",
xlab="Time",lwd=2,ylim=c(0,sum(Rt[1,]))) 
lines(t,Rt[,1],col=2)
lines(t,Rt[,2],col=4) 
legend("topleft",c("Total C release","C release from pool 1", "C release from pool 2"),
lty=c(1,1,1),col=c(1,2,4),lwd=c(2,1,1),bty="n")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TwopParallelModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("TwopParallelModel14")
### * TwopParallelModel14

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TwopParallelModel14
### Title: Implementation of a two-pool C14 model with parallel structure
### Aliases: TwopParallelModel14

### ** Examples

lag <- 2
years=seq(1901+lag,2009,by=0.5)
LitterInput=700 
Ex=TwopParallelModel14(t=years,ks=c(k1=1/2.8, k2=1/35),C0=c(200,5000), 
F0_Delta14C=c(0,0),In=LitterInput, gam=0.7,inputFc=C14Atm_NH,lag=lag)
R14m=getF14R(Ex)
C14m=getF14C(Ex)
C14t=getF14(Ex)
par(mfrow=c(2,1))
plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years, C14t[,1], col=4)
lines(years, C14t[,2],col=4,lwd=2)
legend("topright",c("Delta 14C Atmosphere", "Delta 14C pool 1", "Delta 14C pool 2"),
lty=c(1,1,1),col=c(1,4,4),lwd=c(1,1,2),bty="n")
plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years,C14m,col=4)
lines(years,R14m,col=2)
legend("topright",c("Delta 14C Atmosphere","Delta 14C SOM", "Delta 14C Respired"),
lty=c(1,1,1), col=c(1,4,2),bty="n")
par(mfrow=c(1,1))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TwopParallelModel14", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("TwopSeriesModel")
### * TwopSeriesModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TwopSeriesModel
### Title: Implementation of a two pool model with series structure
### Aliases: TwopSeriesModel

### ** Examples

t_start=0 
t_end=10 
tn=50
timestep=(t_end-t_start)/tn 
t=seq(t_start,t_end,timestep) 
ks=c(k1=0.8,k2=0.4)
a21=0.5
C0=c(C10=100,C20=150)
In = 30
#
Temp=rnorm(t,15,1)
TempEffect=data.frame(t,fT.Daycent1(Temp))
#
Ex1=TwopSeriesModel(t,ks,a21,C0,In,xi=TempEffect)
Ct=getC(Ex1)
Rt=getReleaseFlux(Ex1)
#
plot(t,rowSums(Ct),type="l",ylab="Carbon stocks (arbitrary units)",
xlab="Time (arbitrary units)",lwd=2,ylim=c(0,sum(Ct[1,]))) 
lines(t,Ct[,1],col=2)
lines(t,Ct[,2],col=4) 
legend("bottomright",c("Total C","C in pool 1", "C in pool 2"),
lty=c(1,1,1),col=c(1,2,4),lwd=c(2,1,1),bty="n")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TwopSeriesModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("TwopSeriesModel14")
### * TwopSeriesModel14

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TwopSeriesModel14
### Title: Implementation of a two-pool C14 model with series structure
### Aliases: TwopSeriesModel14

### ** Examples

years=seq(1901,2009,by=0.5)
LitterInput=700 
#
Ex=TwopSeriesModel14(t=years,ks=c(k1=1/2.8, k2=1/35),
C0=c(200,5000), F0_Delta14C=c(0,0),
In=LitterInput, a21=0.1,inputFc=C14Atm_NH)
R14m=getF14R(Ex)
C14m=getF14C(Ex)
C14t=getF14(Ex)
#
par(mfrow=c(2,1))
plot(C14Atm_NH,type="l",xlab="Year",
ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years, C14t[,1], col=4)
lines(years, C14t[,2],col=4,lwd=2)
legend("topright",c("Delta 14C Atmosphere", "Delta 14C pool 1", "Delta 14C pool 2"),
lty=c(1,1,1),col=c(1,4,4),lwd=c(1,1,2),bty="n")
#
plot(C14Atm_NH,type="l",xlab="Year",ylab="Delta 14C (per mil)",xlim=c(1940,2010)) 
lines(years,C14m,col=4)
lines(years,R14m,col=2)
legend("topright",c("Delta 14C Atmosphere","Delta 14C SOM", "Delta 14C Respired"),
lty=c(1,1,1), col=c(1,4,2),bty="n")
par(mfrow=c(1,1))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TwopSeriesModel14", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("Yasso07Model")
### * Yasso07Model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Yasso07Model
### Title: Implementation of the Yasso07 model
### Aliases: Yasso07Model

### ** Examples

years=seq(0,50,0.1) 
C0=rep(100,5)
In=0

Ex1=Yasso07Model(t=years,C0=C0,In=In)
Ct=getC(Ex1)
Rt=getReleaseFlux(Ex1)

plotCPool(years,Ct,col=1:5,xlab="years",ylab="C pool",
ylim=c(0,max(Ct)))
legend("topright",c("xA","xW","xE","xN","xH"),lty=1,col=1:5,bty="n")

plotCPool(years,Rt,col=1:5,xlab="years",ylab="Respiration",ylim=c(0,50))
legend("topright",c("xA","xW","xE","xN","xH"),lty=1,col=1:5,bty="n")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Yasso07Model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("YassoModel")
### * YassoModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: YassoModel
### Title: Implementation of the Yasso model.
### Aliases: YassoModel

### ** Examples

years=seq(0,500,0.5) 
C0=rep(100,7)
#
Ex1=YassoModel(t=years,C0=C0)
Ct=getC(Ex1)
Rt=getReleaseFlux(Ex1)
#
plotCPool(years,Ct,col=1:7,xlab="years",ylab="C pool",ylim=c(0,200))
legend("topright",c("fwl","cwl","ext","cel","lig","hum1","hum2"),lty=1,col=1:7,bty="n")
#
plotCPool(years,Rt,col=1:7,xlab="years",ylab="Respiration",ylim=c(0,50))
legend("topright",c("fwl","cwl","ext","cel","lig","hum1","hum2"),lty=1,col=1:7,bty="n")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("YassoModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("bacwaveModel")
### * bacwaveModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bacwaveModel
### Title: Implementation of the microbial model Bacwave (bacterial waves)
### Aliases: bacwaveModel

### ** Examples

hours=seq(0,800,0.1)
#
#Run the model with default parameter values
bcmodel=bacwaveModel(t=hours)
Cpools=getC(bcmodel)
#
#Time solution
matplot(hours,Cpools,type="l",ylab="Concentrations",xlab="Hours",lty=1,ylim=c(0,max(Cpools)*1.2))
legend("topleft",c("Substrate", "Microbial biomass"),lty=1,col=c(1,2),bty="n")
#
#State-space diagram
plot(Cpools[,2],Cpools[,1],type="l",ylab="Substrate",xlab="Microbial biomass")
#
#Microbial biomass over time
plot(hours,Cpools[,2],type="l",col=2,xlab="Hours",ylab="Microbial biomass")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bacwaveModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("by_PoolIndex-function-character-character-method")
### * by_PoolIndex-function-character-character-method

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: by_PoolIndex,function,character,character-method
### Title: convert a function f of to f_vec
### Aliases: by_PoolIndex,function,character,character-method

### ** Examples

leaf_resp=function(leaf_pool_content){leaf_pool_content*4}
leaf_resp(1)
poolNames=c(
   "some_thing"
  ,"some_thing_else"
  ,"some_thing_altogther"
  ,"leaf_pool_content"
)
leaf_resp_vec=by_PoolIndex(leaf_resp,poolNames,timeSymbol='t')
# The result is the same since the only the forth position in the vector
leaf_resp_vec(c(1,27,3,1),5) 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("by_PoolIndex-function-character-character-method", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("eCO2")
### * eCO2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: eCO2
### Title: Soil CO2 efflux from an incubation experiment
### Aliases: eCO2
### Keywords: datasets

### ** Examples

head(eCO2)

plot(eCO2[,1:2],type="o",ylim=c(0,50),ylab="CO2 efflux (ug C g-1 soil day-1)")
arrows(eCO2[,1],eCO2[,2]-eCO2[,3],eCO2[,1],eCO2[,2]+eCO2[,3], angle=90,length=0.3,code=3)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("eCO2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("entropyRatePerJump")
### * entropyRatePerJump

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: entropyRatePerJump
### Title: Entropy rate per jump
### Aliases: entropyRatePerJump

### ** Examples

B6=matrix(c(-1,1,0,0,-1,1,0,0,-1),3,3); u6=matrix(c(1,0,0))
entropyRatePerJump(A=B6, u=u6)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("entropyRatePerJump", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("entropyRatePerTime")
### * entropyRatePerTime

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: entropyRatePerTime
### Title: Entropy rate per time
### Aliases: entropyRatePerTime

### ** Examples

B6=matrix(c(-1,1,0,0,-1,1,0,0,-1),3,3); u6=matrix(c(1,0,0))
entropyRatePerTime(A=B6, u=u6)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("entropyRatePerTime", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("incubation_experiment")
### * incubation_experiment

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: incubation_experiment
### Title: Soil CO2 efflux from an incubation experiment, along with the
###   soil mass and carbon concentration measurements.
### Aliases: incubation_experiment
### Keywords: datasets

### ** Examples

eCO2=incubation_experiment$eCO2
head(eCO2)

plot(eCO2[,1:2],type="o",ylim=c(0,50),ylab="CO2 efflux (ug C g-1 soil day-1)")
arrows(eCO2[,1],eCO2[,2]-eCO2[,3],eCO2[,1],eCO2[,2]+eCO2[,3], angle=90,length=0.3,code=3)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("incubation_experiment", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("linearScalarModel")
### * linearScalarModel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: linearScalarModel
### Title: Implementation of a general model for linear non-autonomous
###   systems with scalar modifiers
### Aliases: linearScalarModel

### ** Examples

t=seq(0,52*200,1) # Fix me! Add an example.  



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("linearScalarModel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("listProduct")
### * listProduct

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: listProduct
### Title: tensor product of lists
### Aliases: listProduct

### ** Examples

listProduct(list('a','b'),list(1,2))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("listProduct", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pathEntropy")
### * pathEntropy

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pathEntropy
### Title: Path Entropy
### Aliases: pathEntropy

### ** Examples

B6=matrix(c(-1,1,0,0,-1,1,0,0,-1),3,3); u6=matrix(c(1,0,0))
pathEntropy(A=B6, u=u6)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pathEntropy", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
