from Rexample import *
from sympy import symbols
class C14example(Rexample):
    def __init__(self,name,matrix,iv,iF,inputrates,c14fraction):
        super(C14example,self).__init__(name,matrix,iv,inputrates)
        self.name=name+"_c14"
        self.c14fraction=c14fraction
        self.trunk="runit."+name
        self.addanls14()
        self.iF=iF
####################################################################################
    def addanls14_fromAbsoluteFractionModern(self,AFM,F0_AFM):
        m,inputrates,c_sym_strs,n= self.matrix,self.inputrates,self.c_sym_strs,self.n
        th=5730
        k=log(Rational(1,2))/th
        I=eye(n)
        mn=m+k*I
        t= Symbol("t")
        tau= Symbol("tau")
        self.anls14=(mn*t).exp()*F0_AFM.multiply_elementwise(Matrix(self.c_sym_strs))+((mn*tau).exp()*inputrates*AFM).integrate((tau,0,t))
        self.anlsF14=zeros(n,1)
        # compute ration and convert to Delta14C format
        for j in range(self.n):
            self.anlsF14[j]=(self.anls14[j]/self.anls[j]-1)*1000
####################################################################################
    def addanls14(self):
        self.analyticCandResp()
        AFM=self.c14fraction
        F0=self.symbolicF0
        self.addanls14_fromAbsoluteFractionModern(AFM,F0)

####################################################################################
    @property
    def symbolicF0(self):	
        n=self.n
        f_sym_strs=[]
        f_sym=zeros(n,1)
        symbolprefix="f0"
        for i in range(n):
            fs=symbolprefix+str(i+1)
            f_sym_strs.append(fs)
        f_syms=symbols(f_sym_strs)
        self.f_sym_strs=f_sym_strs	
        #F0=zeros(n)
        #for i in range(n):
        #    F0[i,i]=f_syms[i]
        F0=Matrix(f_syms)
        return(F0)

####################################################################################
    def setUpVars(self):
        pp=super(C14example,self)
        Text=pp.setUpVars()
        for j in range(self.n):       
           Text+=(self.shift+self.f_sym_strs[j]+"="+str(self.iF[j])+"\n")
        Text+="\
   initialF=ConstFc("+rlistprint(self.f_sym_strs,self.shift)+",\n format=\"AbsoluteFractionModern\")\n\
   Fc=BoundFc(function(t){"+str(self.c14fraction)+"},t_start,t_end,format=\"AbsoluteFractionModern\")\n\
   th=5730\n\
   k=log(0.5)/th\n"

        return(Text)
####################################################################################
    def sols(self):
        pp=super(C14example,self)
        Text=pp.sols()
        Text+="\
   Y14=matrix(ncol="+str(self.n)+",nrow=length(t))\n"

        for j in range(self.n):
           Text+=(self.shift+"Y14[,"+str(j+1)+"]="+str(self.anls14[j])+"\n")
        # add the F14 part
        Text+="\
   F14=matrix(ncol="+str(self.n)+",nrow=length(t))\n"

        for j in range(self.n):
           Text+=(self.shift+"F14[,"+str(j+1)+"]="+str(self.anlsF14[j])+"\n")

        return(Text)
####################################################################################
    def setUpModel(self):
        Text="\
   mod=GeneralModel_14(\n\
    t=t,\n\
    A=A,\n"\
       "ivList=" +rlistprint(self.c_sym_strs,self.shift)+",\n"\
        "initialValF="+self.shift+"initialF,\n"\
        "inputFluxes="+self.shift+"inputrates,\n"\
        "inputFc="+self.shift+"Fc,\n"\
        "di="+self.shift+"k,\n"\
        "solverfunc="+self.shift+"deSolve.lsoda.wrapper\n   )\n\
   Y14ode=getC14(mod) \n\
   F14ode=getF14(mod) \n\
   Yode=getC(mod) \n\
   Rode=getReleaseFlux(mod) \n"
        return(Text)
####################################################################################
    def plots(self):
        n=self.n
        pp=super(C14example,self)
        Text=pp.plots()
        Text+="\
   plot(t,Y14[,1],type=\"l\",lty=lt1,col=1,ylab=\"14C-Concentrations\",xlab=\"Time\",ylim=c(min(Y14),max(Y14)))\n"

        Text+=(self.shift+"lines(t,Y14ode[,1],type=\"l\",lty=lt2,col=1)\n")
        collist="c(1,1"
        for j in range(2,n+1):
           colstr=str(j)
           Text+=(self.shift+"lines(t,Y14[,"+str(j)+"],type=\"l\",lty=lt1,col="+colstr+")\n")
           collist+=","+colstr
           Text+=(self.shift+"lines(t,Y14ode[,"+str(j)+"],type=\"l\",lty=lt2,col="+colstr+")\n")
           collist+=","+colstr
        collist+=")"
        Text+="\
   plot(t,F14[,1],type=\"l\",lty=lt1,col=1,ylab=\"14C-C ratio \",xlab=\"Time\",ylim=c(min(F14,F14ode),max(F14,F14ode)))\n"

        Text+=(self.shift+"lines(t,F14ode[,1],type=\"l\",lty=lt2,col=1)\n")
        collist="c(1,1"
        for j in range(2,n+1):
           colstr=str(j)
           Text+=(self.shift+"lines(t,F14[,"+str(j)+"],type=\"l\",lty=lt1,col="+colstr+")\n")
           collist+=","+colstr
           Text+=(self.shift+"lines(t,F14ode[,"+str(j)+"],type=\"l\",lty=lt2,col="+colstr+")\n")
           collist+=","+colstr
        collist+=")"

        Text+="\
   legend(\n\
   \"topright\",\n\
     c(\n"
        
        for j in range(1,self.n):       
           Text+=(self.shift+"  \"anlytic sol for pool "+str(j)+"\",\n")
           Text+=(self.shift+"  \"numeric sol for pool "+str(j)+"\",\n")
              
        Text+=(self.shift+"  \"anylytic sol for pool "+str(self.n)+"\",\n")
        Text+=(self.shift+"  \"numeric sol for pool "+str(self.n)+"\"\n")
        Text+="     ),\n\
     lty=c(lt1,lt2),\n\
     col="+collist+"\n\
   )\n"
        return(Text)
####################################################################################
    def checks(self):
        n=self.n
        pp=super(C14example,self)
        Text=pp.plots()
        Text="\
# begin checks \n\
   tol=.02*max(Y14)/tn\n\
   checkEquals(\n\
    Y14,\n\
    Y14ode,\n\
    \"test numeric solution for 14C-Content computed by the ode mehtod against analytical\",\n\
    tolerance = tol,\n\
   )\n\
   checkEquals(\n\
    F14,\n\
    F14ode,\n\
    \"test numeric solution for F14 computed by the ode mehtod against analytical\",\n\
    tolerance = tol,\n\
   )\n"
        return(Text)

