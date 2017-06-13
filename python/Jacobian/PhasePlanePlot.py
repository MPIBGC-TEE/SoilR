#!/usr/bin/python3
# vim: set expandtab ts=4
from matplotlib.backends.backend_pdf import PdfPages
from mpi4py import MPI
import numpy as np
import matplotlib.pyplot as plt
from evalOnGrid import evalOnGrid
from scipy.integrate import odeint

def myrange(rank,size,arr):
    l=len(arr)
    slicewidth=l/size
    print(rank)
    print(l)
    print(int(slicewidth*rank))
    if rank<size:
        res=arr[int(slicewidth*rank):int(slicewidth*(rank+1))]
    else:
        res=arr[int(slicewidth*rank):]
    return(res)

def plotPhasePlane(parDict,X,X_dot):
    x_dot,y_dot=X_dot
    x,y=X
    comm = MPI.COMM_WORLD
    size = comm.Get_size()
    rank = comm.Get_rank()
    if rank == 0:
        pp = PdfPages('multipage.pdf')
    
    
    x_dot_par=x_dot.subs(parDict)
    y_dot_par=y_dot.subs(parDict)
        
    # define the system dy/dt = f(y, t)
    def f(X,t):
        xval=X[0]
        yval=X[1]
        xdv=x_dot_par.subs({x:xval,y:yval})
        ydv=y_dot_par.subs({x:xval,y:yval})
        return([xdv,ydv])
    
    tf=np.linspace(0, 2, 16)   # time grid forward
    tb=np.linspace(0,-2, 16)   # time grid backwards
    x_min= 0
    x_max= 10000
    y_min= 0
    y_max= 10000
    res=(x_max-x_min)/10.0
    X = np.arange(x_min, x_max, res)
    Y = np.arange(y_min, y_max, res)
    Xm, Ym = np.meshgrid(X, Y)
    #startValues=[[i,j] for i in X for j in Y]
    stepsize=4
    startValues=[[X[i],Y[j]] for i in range(0,len(X),stepsize) for j in range(0,len(Y),stepsize)]
    myStartValues=myrange(rank,size,startValues)
    mydata=[]
    for X0 in myStartValues:
        soln=odeint(f,X0,tf)
        mydata.append(soln)
        soln=odeint(f,X0,tb)
        mydata.append(soln)
    #data = (rank+1)**2
    data = comm.gather(mydata, root=0)
    if rank == 0:
        print("rank="+str(rank))
        f1=plt.figure()
        for solutionSet in data:
            for soln in solutionSet:
                xvals=soln[:,0]
                yvals=soln[:,1]
                plt.plot(xvals,yvals,color="k")
        U= evalOnGrid(Xm,Ym,x_dot_par,[x,y])
        V =evalOnGrid(Xm,Ym,y_dot_par,[x,y])
        Q = plt.quiver( Xm,Ym, U, V,pivot="tip",units="width")
        #nullclines
        Cx=plt.contour(X,Y,U,levels=[0],colors=["r"])
        Cy=plt.contour(X,Y,V,levels=[0],colors=["g"])
        plt.title("A=")
        plt.xlim(x_min,x_max)
        plt.ylim(y_min,y_max)
        pp.savefig(f1)
    else:
        assert data is None
    	
    if rank == 0:
        pp.close()
