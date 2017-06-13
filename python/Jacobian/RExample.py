#!/usr/bin/python3
# vim:set ff=unix expandtab ts=4 sw=4:
from subprocess import call
from sympy import re, eye, diff, Matrix
from python2R import Rcode, dict2assignments, matrix2matrix
import pathlib
import os
import shutil
from string import Template
from Latex import *
import re as regexp
from subprocess import call
from mpi4py import MPI
import sympy as sp
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import axes3d
from numpy import array,linspace,meshgrid,arange
from scipy.integrate import odeint
from scipy.optimize import fsolve,root
from matplotlib.backends.backend_pdf import PdfPages
import math
from evalOnGrid import evalOnVecList
from coordArrays import tuplelist
from itertools import chain
from PhasePlanePlot import plotPhasePlane
comm = MPI.COMM_WORLD
size = comm.Get_size()
rank = comm.Get_rank()
def flatten(lst):
    if lst:
        ch=chain.from_iterable(lst)
    #chain=chain.from_iterable(lst)
        return(list(ch))
def myrange(myrank,arr):
    slicewidth=int(len(arr)/size)
    if myrank<size:
        res=arr[slicewidth*rank:slicewidth*(rank+1)]
    else:
        res=arr[slicewidth*rank:]
    return(res)
class DecompositionUndecidability(BaseException):
    pass
class RcodeException(BaseException):
    pass
class SOMCreationException(BaseException):
    pass
class NegativeRespiration(BaseException):
    pass
def getRecipient(key):
	return(int(key.split("_to_")[1]))
def jacobian(vec,sym):
	def dd(i,j):
		return(diff(vec[i],sym[j]))
	return(Matrix(len(vec),len(sym),dd))

def getSender(key):
	return(int(key.split("_to_")[0]))

class RExample:
    def __init__(self,C,alphas,F,I,stem):
        self.C=C
        self.F=F
        self.alphas=alphas
        self.I=I
        self.stem=str(stem)
        self.dirPath=Path("output").joinpath(stem)
        self.parameterSets=list()
        if rank==0:
            if self.dirPath.exists():
                shutil.rmtree(self.dirPath.as_posix())
            self.dirPath.mkdir(parents=True)
        self.L=Latex(self.dirPath.as_posix(),stem)
        self.fp=list()#suggested fixed points
        self.nr,self.nc=self.C.shape

    def cvecAssignments(self):
        cvec=self.C
        if self.nc!=1:
            raise "C is not a vector"
        text="\n".join([Template("${sym}=C[${ri}]").substitute(sym=Rcode(cvec[i]),ri=i+1) for i,j in enumerate(cvec)])
        return(text)
    
    def getTransferMatrix(self):
        T=-1*eye(self.nr)
        keys=self.alphas.keys()	
        for key in keys:
    	    T[getRecipient(key)-1,getSender(key)-1]=self.alphas[key]
        return(T)

    def alphaFuncs(self):
        text="alpha_funcs=list() \n"
        text+="\n".join([Template("alpha_funcs[[\"${key}\"]]=function(C,t){\n \
        ${cvec} \n \
        ${val}  \n } \n").substitute(key=key,val=val,cvec=self.cvecAssignments()) for key,val in self.alphas.items()])
        return(text)

    def getN(self):
        ks=[self.F[i]/self.C[i] for i in range(len(self.C))]
        tv=False
        try:
            tv=any([k<0 for k in ks])
        except:
            print("could not determine if the entries in Matrix N are all positive. This is probably due to a lack of information about some of the model parameters. Try to increase this information by making explicit assumptions when creating symbols like e.g. k=symbol('k',positive=True)")
            N=diag(*ks)
            return(N)
        if tv:
           raise(SOMCreationException("Found a negative decay rate. Either check the definition of F or the assumptions about its parameters"))
        N=diag(*ks)
        return(N)
    def getOperator(self):	
        T=self.getTransferMatrix()
        f=self.F
        Op=T*f
        return(Op)

    def jacobian(self):
        Op=self.getOperator()
        return(jacobian(Op,self.C))

    def suggestFixedPoint(self,vec):
        self.fp.append(vec) 
        
    def addParameterSet(self,dic):
        self.parameterSets.append(dic)
    def computeNumericalFixedPoints(self): 
        for i in range(len(self.parameterSets)):
            dic=self.parameterSets[i]
        #for dic in self.parameterSets:
            Cdot=self.I+self.getOperator()
            CdotNum=Cdot.subs(dic)
            pprint(CdotNum)
            

            JNum=self.jacobian().subs(dic)
            def evalJ(intup):
                # read from the input tuple
                dic={self.C[i]:intup[i] for i in range(0,len(intup))}
                M=JNum.subs(dic)
                return(M)
            def evalJarr(intup):
                M=evalJ(intup)
                return(array(M.tolist(),dtype="float"))
            #Jeval=lambdify(tuple(C),JNum,use_array=True)
            def evalCdotNum(intup):
                # read from the input tuple
                dic={self.C[i]:intup[i] for i in range(0,len(intup))}
                outtup=tuple(CdotNum.subs(dic))
                return(outtup)

            startValues=dic["startValuesForFixedPointSearch"]
            myStartValues=myrange(rank,startValues)
            #print("myStartValues")
            #print(myStartValues)
            mySols=list()
            for starttuple in myStartValues:
                sol=root(evalCdotNum, starttuple, method="hybr", jac=evalJarr)
                if sol.success:
                    #print("sol.x")
                    #print(sol.x)
                    #if not(sol.x in mySols):
                    mySols.append(sol.x)
            print("mySols")        
            print(mySols)        
            # now compute the jacobian at the fixed points
            myUnstable=list()
            myStable=list()
            myUndecided=list()
            for sol in mySols:
                ev=list((evalJ(sol)).eigenvals().keys())
                if any([re(v)>0 for v in ev]):
                    myUnstable.append(sol)
                elif all([re(v)<0 for v in ev]):
                    myStable.append(sol)
                else:    
                    myUndecided.append(sol)
            solsList= comm.gather(mySols, root=0)
            #sols=[sol  for l in solsList for sol in l]
            unstableList= comm.gather(myUnstable, root=0)
            stableList= comm.gather(myStable, root=0)
            undecidedList= comm.gather(myUndecided, root=0)
            sols,unstable,stable,undecided=map(flatten,[solsList,unstableList,stableList,undecidedList])
            # remove empty lists (coming from nodes with no findings)
            if rank == 0:
                font = {'family' : 'serif',
                        'color'  : 'darkred',
                        'weight' : 'normal',
                        'size'   : 16,
                        }
                print("rank="+str(rank))
                print(len(sols))
                print(len(stable))
                print(len(unstable))
                print(len(undecided))
                ranges=dic["plotRanges"]
                vectors=[ linspace(l[0],l[1],200) for l in ranges] 
                xvals=tuplelist(vectors)
                yvals=evalOnVecList(xvals,CdotNum,self.C)
                pp = PdfPages(self.dirPath.joinpath("pythonPlots.pdf").as_posix())
                f1=plt.figure()
                plt.title('Cdot[0] against C[0]', fontdict=font)
                plt.plot([v[0] for v in xvals],[v[0] for v in yvals])
                plt.plot([s[0] for s in unstable],[0 for s in unstable],"ro")
                plt.plot([s[0] for s in stable],[0 for s in stable],"go")
                plt.plot([s[0] for s in undecided],[0 for s in undecided],"go")
                plt.xlabel('C[0]', fontdict=font)
                plt.ylabel('Cdot[0]', fontdict=font)
                #plt.xlim(x_min,x_max)
                #plt.ylim(min(yvals),max(yvals))
                pp.savefig(f1)
                pp.close()
                dic["numericFixedPoints"]={"stable":stable,"unstable":unstable,"undecided":undecided}
                self.parameterSets[i]=dic
                
            else:
                assert sols is None
                assert unstable is None
                assert stable  is None
                assert undecided is None
    
    def opStr(self,Mat,name):
        C_sym=self.C.free_symbols
        Mat=self.getTransferMatrix()
        inter=(C_sym.intersection(Mat.free_symbols))
        print(inter)
        if len(inter)>0:
            str=name+"(C)"
        else:
            str=name
        return(str)    
    def latexSymbolicOperatorComponents(self):
        L=self.L
        L.addText("Legend:\\\\")
        C_sym=self.C.free_symbols
        T=self.getTransferMatrix()
        F=self.F
        message=""
        try:
            N=self.getN()
            A=T*N
        except(SOMCreationException):
            message="{\\color{red} The entries of N point to SOM Creation (not Decomposition))}"

        L.addDisplayMath("\\mathbf{\\dot{C}}=\\mathbf{I + $Tstr\\cdot F(C)}",Tstr=self.opStr(T,"T"))
        L.addDisplayMath("\\mathbf{\\dot{C}}=${O}",O=self.getOperator()+self.I)
        L.addText("with:\\\\")
        L.addEquationArray([
            "\\mathbf{\\dot{C}}&=&\\frac{d}{d{t}}${C}",
            "\\mathbf{$Tstr}&=&${T}",
            "\\mathbf{$Fstr}&=&${F} \\text{or equivalently} ",
            "\\mathbf{$Nstr}&=&${N} \\text{$m}",
            "\\mathbf{$Astr}&=&${A} \\text{$m}",
            "\\mathbf{I}&=&${I}"],
            C=self.C,
            T=T,Tstr=self.opStr(T,"T"),
            F=F,Fstr=self.opStr(F,"F"),
            N=N,Nstr=self.opStr(N,"N"),
            A=A,Astr=self.opStr(A,"A"),
            m=message,
            I=self.I)
        L.addText("check alphas:\\\\")
        for j in range(self.nr):
            L.addDisplayMath("r_{$j}=-\\sum_{k=1}^{n}T_{k,$j}=$tk",j=j,tk=simplify(-sum(T[:,j])))
    def latexNumericOperatorComponents(self):
        if self.parameterSets:
            for dic in self.parameterSets:
                L=self.L
                L.addText("Legend:\\\\")
                C_sym=self.C.free_symbols
                T=self.getTransferMatrix()
                F=self.F
                raiseSOM=False
                try:
                    N=self.getN()
                except(SOMCreationException):
                    message="{\\color{red} The entries of N point to SOM Creation (not Decomposition))}"
                    raiseSOM=True
                O=self.getOperator()
                I=self.I
                message=""


                L.addDisplayMath("\\mathbf{\\dot{C}}=\\mathbf{I + $Tstr\\cdot F(C)}",Tstr=self.opStr(T,"T"))
                L.addDisplayMath("\\mathbf{\\dot{C}}=${dotC}",dotC=O.subs(dic)+I.subs(dic))
                L.addText("with:\\\\")
                L.addEquationArray([
                    "\\mathbf{\\dot{C}}&=&\\frac{\\partial}{\\partial{t}}${C}",
                    "\\mathbf{$Tstr}&=&${T}",
                    "\\mathbf{$Fstr}&=&${F} \\text{or equivalently} ",
                    "\\mathbf{$Nstr}&=&${N} \\text{$m}",
                    "\\mathbf{I}&=&${I}"],
                    C=self.C,
                    T=T.subs(dic),Tstr=self.opStr(T,"T"),
                    F=F.subs(dic),Fstr=self.opStr(F,"F"),
                    N=N.subs(dic),Nstr=self.opStr(N,"N"),
                    m=message,
                    I=self.I.subs(dic))
                L.addText("check alphas:\\\\")
                for j in range(self.nr):
                    rj=simplify(-sum(T[:,j]).subs(dic))
                    L.addDisplayMath("r_{$j}=\\sum_{k=1}^{n}T_{k,$j}=$rj",j=j,rj=rj)
                    try: 
                        tv=(rj>=0 and rj<=1)
                        print(tv)
                        if tv:
                            L.addDisplayMath("0 \\le r_{$j} \\le 1",j=j)
                        if not(tv):    
                            L.addText("{\\color{red} warning}") 
                            L.addDisplayMath("0 \\le r_{$j} \\le 1 is not true",j=j)
                            raise(NegativeRespiration)
                    except(TypeError):
                        L.addText("cant determine if") 
                        L.addDisplayMath("0 \\le r_{$j} \\le 1",j=j)
                if raiseSOM:
                    #reraise after altering report.
                    raise(SOMCreationException)

    def latexSymbolicJacobian(self):
        L=self.L
        L.addText("\\large{Symbolic Jacobian:}\\\\")
        L.addDisplayMath("\\frac{\\partial}{\\partial\\mathbf{C}}\\left(\\mathbf{ T\\cdot F(C)}\\right)=${J}",J=self.jacobian())
        if len(self.fp)>0:
            for fp in self.fp:
                L.addText("at the fixed Point(s):\\\\")
                J_sym=simplify(self.jacobian().subs(fp))
                L.addDisplayMath("\\frac{\\partial}{\\partial\\mathbf{C}}\\left(\\mathbf{ T\\cdot F(C)}\\right)=${J}",J=J_sym)
                L.addText("Note that a symbolic eigenvalue computation will implicitly assume that $$\\mathbf{J}$$ is diagonalizable which could be wrong for some parameter values, so at least the corresponding eigenvectors should be regarded very cautiously. The potential symbolic eigenvalues are:\\\\")
                sym_e=simplify(J_sym.eigenvals())
                pprint(sym_e)
                L.addDisplayMath("${e}",e=sym_e)
    def latexNumericJacobian(self):
        L=self.L
        if self.parameterSets:
            L.addText("\\large{Jacobian for Parameter sets:}\\\\")
        if len(self.fp)>0:
            for fp in self.fp:
                for dic in self.parameterSets:
                    L.addText("with parameters substituted as follows:\\\\")
                    L.addText("\\begin{tabular}{l|l}")
                    L.addText("Name & value \\\\")
                    for key,value in dic.items():
                        L.addText("$$ ${key} $$ & $$ ${value} $$ \\\\",key=key,value=value)
                    L.addText("\\end{tabular}")
                    L.addDisplayMath("\\frac{\\partial}{\\partial\\mathbf{C}}\\left(\\mathbf{ T\\cdot F(C)}\\right)=${J}",J=self.jacobian().subs(fp).subs(dic))
                    L.addText("numerical fixed point values")
                    for key,value in fp.items():
                        if isinstance(value,Expr):
                            L.addDisplayMath("${p}",p=value.subs(dic))
                        else:
                            L.addDisplayMath("${p}",p=value)
                    try:
                        evs=(self.jacobian().subs(fp).subs(dic)).evalf().eigenvals()
                        evk=evs.keys()
                        L.addText("The eigenvalues are:\\\\")
                        L.addText("\\begin{tabular}{l|l}")
                        L.addText("Eigenvalue & algebraic multiplicity\\\\")
                        for i,j in enumerate(evs):
                            L.addText("\\hline $$ ${ev} $$ & $$ ${ma} $$ \\\\",ev=j.evalf(),ma=evs[j])
                        L.addText("\\end{tabular}")
                        L.addText("\\\\ The stiffness ratio S, defined as \\\\")
                        L.addDisplayMath("S=\\frac{\\max |\\operatorname{Re}(\\vec{\\lambda})|} {\\min |\\operatorname{Re}(\\vec{\\lambda})|}")
                        AbsRealParts=[Abs(re(ev)).evalf() for ev in evk] #keyword: python list comprehension 
                        print(AbsRealParts)
                        try:
                            r=max(AbsRealParts)/min(AbsRealParts)
                            print(r)
                            L.addText("has a numerical value of: $$ ${r} $$ \\\\",r=r)
                        except:
                            L.addText("can not be computed in this case.\\\\")
                        n= Matrix(len(evk),1,list(evk)).norm(2).evalf()
                        pprint(n)# too long as a latex if
                        L.addText("The norm of the vector of eigenvalues is:$$ ${n} $$\\\\ \\clearpage",n=n) #2 norm (usual Frobenius)
                    except:
                        L.addText("The eigenvalues could  not be computed in this case.\\\\")

    def latexSymbolicFixedPoints(self):
        L=self.L
        if len(self.fp)>0:
            for fp in self.fp:
                L.addText("suggested symbolica fixed Point(s):\\\\")
                L.addDisplayMath("\\mathbf{\\tilde{C}}=${fp}",fp=self.C.subs(fp))
                L.addDisplayMath("\\mathbf{\\dot{C}}(\\mathbf{\\tilde{C}})=${O}",O=simplify((self.getOperator()+self.I).subs(fp)))

    def latexNumericFixedPoints(self):
        L=self.L
        T=self.getTransferMatrix()
        F=self.F
        if len(self.parameterSets)>0:
            for dic in self.parameterSets:
                # first handle user specified fixed points
                if len(self.fp)>0: 
                    for fp in self.fp:
                        L.addText("\\large{Numerical values for user suggested fixed Point(s) for the parameter sets:}\\\\")
                        L.addDisplayMath("\\mathbf{\\tilde{C}}=${fp}",fp=self.C.subs(fp).subs(dic))
                        L.addText("test:\\\\")
                        L.addDisplayMath("\\mathbf{ $Tstr\\cdot $Fstr+I}=${O}",
                        O=self.getOperator().subs(fp).subs(dic)+self.I.subs(dic),
                        Tstr=self.opStr(T,"T"),
                        Fstr=self.opStr(F,"F")
                        )
                    # now handle numerically derived fixed points
                if "numericFixedPoints" in dic.keys():
                    L.addText("\\large{Numerically found fixed Point(s) for the parameter sets:}\\\\")
                    L.addText("stable fixed Points (only eingenvalues with negative real part:\\\\")
                    for fp in dic["numericFixedPoints"]["stable"]:
                        L.addDisplayMath("\\mathbf{\\tilde{C}}=${fp}",fp=Matrix(fp))
                        L.addText("test:\\\\")
                        L.addDisplayMath("\\mathbf{ $Tstr\\cdot $Fstr+I}=${O}",
                        O=self.getOperator().subs({self.C[i]:fp[i] for i in range(len(self.C))}).subs(dic)+self.I.subs(dic),
                        Tstr=self.opStr(T,"T"),
                        Fstr=self.opStr(F,"F")
                        )
                    L.addText("Unstable fixed Points (only eingenvalues with positive real part:\\\\")
                    for fp in dic["numericFixedPoints"]["unstable"]:
                        L.addDisplayMath("\\mathbf{\\tilde{C}}=${fp}",fp=Matrix(fp))
                        L.addText("test:\\\\")
                        L.addDisplayMath("\\mathbf{ $Tstr\\cdot $Fstr+I}=${O}",
                        O=self.getOperator().subs({self.C[i]:fp[i] for i in range(len(self.C))}).subs(dic)+self.I.subs(dic),
                        Tstr=self.opStr(T,"T"),
                        Fstr=self.opStr(F,"F")
                        )

                    L.addText("undecided fixed Points (eingenvalues not deceicive:\\\\")
                    for fp in dic["numericFixedPoints"]["undecided"]:
                        L.addDisplayMath("\\mathbf{\\tilde{C}}=${fp}",fp=Matrix(fp))
                        L.addText("test:\\\\")
                        L.addDisplayMath("\\mathbf{ $Tstr\\cdot $Fstr+I}=${O}",
                        O=self.getOperator().subs({self.C[i]:fp[i] for i in range(len(self.C))}).subs(dic)+self.I.subs(dic),
                        Tstr=self.opStr(T,"T"),
                        Fstr=self.opStr(F,"F")
                        )

    def latexPhasePlanePlot(self):
        F=self.F
        if len(F)==2:
            # try to produce a 3D plot of Cdot 
            pass
            if self.parameterSets:
                C=self.C
                O=self.getOperator()
                print(O)
                I=self.I
                for dic in self.parameterSets:
                  
                    if "C_Ranges" in dic.keys():
                        C_Ranges=dic["C_Ranges"]
                        # C_Ranges is too long for the subsequent substitutions
                        # and also not necessary so we remove it
                        # this should be done more elegantly later at all places
                        del(dic["C_Ranges"])
                        Cdot=O.subs(dic)+I.subs(dic)
                        C=self.C
                        pprint(Cdot)
                        pprint(C)
                        #fig = plt.figure()
                        #for i in range(0,2):
                        #    fZ=lambdify((C[0],C[1]),Cdot[i])
                        #    #fZ=lambdify(C
                        #    X = C_Ranges[0]
                        #    Y = C_Ranges[1]
                        #    Xm,Ym=meshgrid(X,Y)
                        #    Z_0=fZ(Xm,Ym)
                        #    #ax = fig.gca(projection='3d')
                        #    ax = fig.add_subplot(121, projection='3d')
                        #    ax.set_aspect(1)
                        #    #ax.plot_surface(Xm, Ym, Z_0, rstride=8, cstride=8, alpha=0.3)
                        #    ax.plot_surface(Xm, Ym, Z_0, alpha=0.3)
    
                        ##plt.show()
                        ##orgdir=os.getcwd()
                        ##os.chdir(self.stem)
                        #filename=self.stem
                        #pp = PdfPages('PhasePlanePlot.pdf')
                        #pp.savefig(fig)
                        #pp.close()
                        #plotPhasePlane(dic,C,Cdot)


            

    def latexSummary(self):
        try:
            self.latexSymbolicOperatorComponents()
            self.latexNumericOperatorComponents()
            self.latexSymbolicJacobian()
            self.latexSymbolicFixedPoints()
            self.latexNumericJacobian()
            self.latexNumericFixedPoints()
            self.latexPhasePlanePlot()
            self.L.write()
        except(NegativeRespiration):
            self.L.write()
            raise(NegativeRespiration)
        except(SOMCreationException):
            self.L.write()
            raise(SOMCreationException)

            
            

    def latexComponents(self):
        self.latexSymbolicOperatorComponents()
        self.latexNumericOperatorComponents()
        #self.latexSymbolicJacobian()
        #self.latexNumericJacobian()
        self.latexNumericFixedPoints()
        self.L.write()

    def writeSoilRModel(self):
        testFileName=self.stem+".R"
        dic=self.parameterSets[0]
        print(self.alphas)
        # alphas_v=self.alphas.subs(dic)
        prolog=Template("#!/usr/bin/Rscript  \n \
        # vim:set ff=unix expandtab ts=2 sw=2: \n \
        require(\"methods\") \n \
        require(\"deSolve\") \n \
        require(\"parallel\") \n \
        prefix=\"${soilrRepoBase}/pkg/R/\" \n \
        globstring=paste(prefix,\"*.R\",sep=\"\") \n \
        auto_paths=Sys.glob(globstring) \n \
         \n \
        for (f in auto_paths){ \n \
            source(f,echo=FALSE) \n \
        } \n \
        #------------------------------------------------------------------------- \n ".replace("         ","")).substitute(
            soilrRepoBase=os.environ["soilrRepoBase"]
        )
        funcDef=Template("${name}=function(${varList}){ \n\
            tol=.02/tn \n \
            timestep=(t_end-t_start)/tn \n \
            t=seq(t_start,t_end,timestep) \n \
            nr=${nr}\n \
            ${alphaFuncs} \n\
            #----------------------------------------- \n \
            f=function(C,t){ \n \
              ${cvec}\n \
              return( \n \
              ${F} \n\
              ) \n \
            } \n \
            Anl=new(\"TransportDecompositionOperator\",t_start,Inf,nr,alpha_funcs,f) \n \
            #----------------------------------------- \n \
            iv=as.vector(C_0)\n \
            #----------------------------------------- \n \
            inputrates=BoundInFlux( \n \
              function(t){return(\n \
              ${InFlux} \n \
              )} , \n \
              t_start, \n \
              t_end \n \
            ) \n \
                     \n \
            #----------------------------------------- \n \
            # build the two models (linear and nonlinear) \n \
            modnl=GeneralNlModel( t, Anl, iv, inputrates, deSolve.lsoda.wrapper) \n \
            Cpools=getC(modnl) \n \
            plot(modnl) \n \
        } \n \
        ".replace("         ","")).substitute(
            name=self.stem,
            varList=",".join([Rcode(key) for key in dic.keys()]),
            F=matrix2matrix(self.F),
            cvec=self.cvecAssignments(),
            alphaFuncs=self.alphaFuncs(),
            InFlux=matrix2matrix(self.I),
            nr=self.nr
        )
        calls="\n".join([Template("${name}(${varDict})").substitute(name=self.stem,
        varDict=dict2assignments({k:v for k,v in dic.items() if not (k in ["startValuesForFixedPointSearch","plotRanges","numericFixedPoints"])})) for dic in self.parameterSets])

        text=prolog+funcDef+calls
        orgdir=os.getcwd()
        os.chdir(self.dirPath.as_posix())
        rlog="R.log"
        try:
            f=open(testFileName,"w")
            f.write(text)
            f.close()
           
            f=open(rlog,"w")
            ret=call(["Rscript", testFileName],stdout=f)
            if ret != 0:
                raise RcodeException("The Rcode did not run")
            f.close()
        except: # note that we catch all kind of exeptions because we want to change
                # Back to the right dir in any case
            os.chdir(orgdir)
            raise RcodeException("got an exeption during Rcode execution")
        os.chdir(orgdir)


if __name__ == '__main__':
    twoPMicrobial()	
    #Wang2014=Example({"C_1":"C_s","C_2":"C_b"})
