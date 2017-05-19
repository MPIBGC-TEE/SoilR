from Rexample import *
class Manzoniexample(Rexample):
# this class produces SoilR test that reflect the nomenclature of the manzoni paper
# and allow to specify the analytic solutions that can not be computed by sympy

    def __init__(self,name,matrix,c_sym,meanTransitTime,subslist):
        nr=matrix.rows 
        inputrates=zeros(nr,1)
        f=lambda el:el.subs(subslist)
        iv=Matrix([f(ci) for ci in c_sym])
        self.iv=iv/iv.norm()
        self.c_sym=c_sym
        self.subslist=subslist
        self.meanTransitTime=meanTransitTime

        super(Manzoniexample,self).__init__(name,matrix,iv,inputrates)
        self.trunk="runit.Manzoni."+name
        self.n=nr
############################################################################
    def setUpVars(self):
        Text=""
        for i in range(len(self.subslist)):
            tup=self.subslist[i]
            print(tup)
            Text+=(self.shift+str(tup[0])+"="+str(tup[1])+"\n")
        Text+="\
   t_start=0\n\
   t_end=2\n\
   tn=100\n\
   tol=.02/tn\n\
   #print(tol)\n\
   timestep=(t_end-t_start)/tn\n\
   t=seq(t_start,t_end,timestep)\n\
   A=new(\"ConstLinDecompOp\","+rmatrixprint(self.matrix,self.shift)+")\n"

        Text+="\
   inputrates=new(\"TimeMap\",t_start,t_end,function(t){return("+rmatrixprint(self.inputrates,self.shift)+")})\n"
        return(Text)

############################################################################
    def analyticCandResp(self):
        sl=self.subslist
        inputrates=self.inputrates.subs(sl)
        m=self.matrix.subs(sl)
        c_sym=self.c_sym
        ck=self.iv
        n=m.rows
        t= Symbol("t")
        tau= Symbol("tau")
        self.anls=(m*t).exp()*c_sym+((m*tau).exp()*inputrates).integrate((tau,0,t))
        testvec=ones(1,n)
        respcoeffs=-testvec*m
        print("respcoeff=\n",respcoeffs)
        self.anlresp=(respcoeffs.transpose()).multiply_elementwise(self.anls)
        self.c_sym_strs=[str(c_i) for c_i in c_sym]
        self.n=n
############################################################################
    def setUpModel(self):
        Text=super(Manzoniexample,self).setUpModel()
        Text+="\
   meanTransitTimeode=getMeanTransitTime(\n\
        A,\n"\
        +rlistprint(self.c_sym_strs,self.shift)\
        +"\n)\n"
        Text+="\
   TTDode=getTransitTimeDistributionDensity(\n\
        A,\n"\
        +rlistprint(self.c_sym_strs,self.shift)\
        +"\n,t\n)\n"
        return(Text)
############################################################################
    def sols(self):
        Text=super(Manzoniexample,self).sols()
        Text+="meanTransitTime="+str(self.meanTransitTime)+"\n"
        return(Text)
############################################################################
    def plots(self):
        Text=super(Manzoniexample,self).plots()
        Text+="\
   plot(t,TTDode,type=\"l\",lty=lt1,col=1,ylab=\"TransitTimeDistributionDensity\",xlab=\"Time\")\n"
        return(Text)
############################################################################
    def checks(self):
        Text=super(Manzoniexample,self).checks()
        Text+="\
   checkEquals(\n\
    meanTransitTime,\n\
    meanTransitTimeode,\n\
    \"test numeric solution for the mean transit Tiye computed by the ode mehtod against analytical value taken from manzoni et al\",\n\
    tolerance = tol,\n\
   )\n"
        return(Text)


