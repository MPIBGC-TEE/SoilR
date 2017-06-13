#
# vim:set ff=unix expandtab ts=4 sw=4:
from functions import *
from sympy.interactive.printing import init_printing
init_printing(use_unicode=True, wrap_line=False, no_global=True)
from sympy.matrices import *
from sympy import Symbol,exp,factorial,log
from sympy import latex
from sympy import oo
from sympy import integrate
import re
from sympy import simplify
from sympy import Basic, Symbol, Integer, C, S, Dummy, Rational, Add, Pow
#from sympy import *
import inspect
import difflib
    
class Rexample(object):
    def __init__(self,name,matrix,iv,inputrates):
        self.shift="   "
        self.name=name
        self.matrix=matrix
        self.iv=iv
        self.inputrates=inputrates
        self.trunk="runit."+name
        #compute the solution
        self.analyticCandResp()

    def analyticCandResp(self):
        inputrates=self.inputrates
        m=self.matrix
        ck=self.iv
        n=m.rows
        c_sym=zeros(n,1)
        c_sym_strs=[]
        t=Symbol("t")
        tau= Symbol("tau")
        #t0= Symbol("t0")
        symbolprefix="c0"
        for i in range(n):
            s=symbolprefix+str(i+1)
            c_sym_strs.append(s)
            c_sym[i,0]=Symbol(s)
        self.c_sym=c_sym
        self.anls=(m*t).exp()*c_sym+((m*tau).exp()*inputrates).integrate((tau,0,t))
        testvec=ones(1,n)
        respcoeffs=-testvec*m
        print("respcoeff=\n",respcoeffs)
        self.anlresp=(respcoeffs.transpose()).multiply_elementwise(self.anls)
        self.ck=ck
        self.c_sym_strs=c_sym_strs
        self.n=n
####################################################################################################    
    def setUpVars(self):
        Text="\
   t_start=0.0\n\
   t_end=2\n\
   tn=100\n\
   tol=.02/tn\n\
   #print(tol)\n\
   timestep=(t_end-t_start)/tn\n\
   t=seq(t_start,t_end,timestep)\n\
   A=new(\"ConstLinDecompOp\","+rmatrixprint(self.matrix,self.shift)+")\n"
        
        for j in range(self.n):       
           Text+=(self.shift+self.c_sym_strs[j]+"="+str(self.ck[j])+"\n")

        Text+="\
   inputrates=new(\"TimeMap\",t_start,t_end,function(t){return("+rmatrixprint(self.inputrates,self.shift)+")})\n"
        return(Text)
####################################################################################################    
    def sols(self):
        Text="\
   Y=matrix(ncol="+str(self.n)+",nrow=length(t))\n"
    
        for j in range(self.n):       
           Text+=(self.shift+"Y[,"+str(j+1)+"]="+str(self.anls[j])+"\n")

        Text+="\
   R=matrix(ncol="+str(self.n)+",nrow=length(t))\n"

        for j in range(self.n):       
           Text+=(self.shift+"R[,"+str(j+1)+"]="+str(self.anlresp[j])+"\n")
        
        return(Text)
####################################################################################################    

    def setUpModel(self):
        print(self.c_sym_strs)
        Text="\
   mod=GeneralModel(\n\
    t,\n\
    A,\n"\
        +rlistprint(self.c_sym_strs,self.shift)\
        +",\n"+self.shift+"inputrates,\n"\
        +self.shift+"deSolve.lsoda.wrapper\n   )\n\
   Yode=getC(mod) \n\
   Rode=getReleaseFlux(mod) \n"
        return(Text)
####################################################################################################    
    def plotprolog(self):
        classname=getattr(self,'__class__')
        trunk="runit."+self.name
        graphicsFileName=trunk+".pdf"
        Text="\
#begin plots \n\
   lt1=2\n\
   lt2=4\n\
   pdf(file=\""+graphicsFileName+"\",paper=\"a4\")\n\
   m=matrix(c(1,2,3,4),4,1,byrow=TRUE)\n\
   layout(m)\n"
        return(Text)
####################################################################################################    
    def plots(self):
        Text="\
   plot(t,Y[,1],type=\"l\",lty=lt1,col=1,ylab=\"Concentrations\",xlab=\"Time\")\n"
        
        Text+=(self.shift+"lines(t,Yode[,1],type=\"l\",lty=lt2,col=1)\n")
        collist="c(1,1"
        for j in range(2,self.n+1):       
           colstr=str(j)
           Text+=(self.shift+"lines(t,Y[,"+str(j)+"],type=\"l\",lty=lt1,col="+colstr+")\n")
           collist+=","+colstr
           Text+=(self.shift+"lines(t,Yode[,"+str(j)+"],type=\"l\",lty=lt2,col="+colstr+")\n")
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
   )\n\
   plot(t,R[,1],type=\"l\",lty=lt1,col=1,ylab=\"Respirationfluxes\",xlab=\"Time\",ylim=c(min(R),max(R)))\n"
        
        Text+=(self.shift+"lines(t,Rode[,1],type=\"l\",lty=lt2,col=1)\n")
        collist="c(1,1"
        for j in range(2,self.n+1):       
           colstr=str(j)
           Text+=(self.shift+"lines(t,R[,"+str(j)+"],type=\"l\",lty=lt1,col="+colstr+")\n")
           collist+=","+colstr
           Text+=(self.shift+"lines(t,Rode[,"+str(j)+"],type=\"l\",lty=lt2,col="+colstr+")\n")
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
####################################################################################################    
    def plotsuffix(self):
        Text="\
   dev.off()\n\
# end plots \n"
        return(Text)
####################################################################################################    
    def checks(self):
        Text="\
# begin checks \n\
   checkEquals(\n\
    Y,\n\
    Yode,\n\
    \"test numeric solution for C-Content computed by the ode mehtod against analytical\",\n\
    tolerance = tol,\n\
   )\n\
   checkEquals(\n\
    R,\n\
    Rode,\n\
    \"test numeric solution for Respiration computed by the ode mehtod against analytical\",\n\
    tolerance = tol,\n\
   )\n"
        return(Text)
####################################################################################################    
        

    def write2file(self):
        #
        name=self.name
        #inputrates,ck,c_sym_strs,n,anls,anlresp= self.inputrates,self.ck,self.c_sym_strs,self.n,self.anls,self.anlresp
        testFileName=self.trunk+".R"
        
        Text="\
# This test function is automatically produced by the python script:"+inspect.getfile(inspect.currentframe())+"\n\
test."+self.name+"=function(){\n\
   require(RUnit)\n"

        Text+=self.setUpVars()\
        
        Text+=self.sols()\
                +self.setUpModel()\
                +self.plotprolog()\
                +self.plots()\
                +self.plotsuffix()\
                +self.checks()

        Text+="\n }"

        f=open(testFileName,"w")
        f.write(Text)
        f.close()
