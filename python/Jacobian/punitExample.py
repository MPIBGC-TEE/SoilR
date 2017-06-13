#!/usr/bin/env python3
# vim:set ff=unix expandtab ts=4 sw=4:
import sys
from sympy import symbols, Symbol, Matrix, oo, limit, assume
from sympy.functions import sin, cos, Abs
from sympy.abc import x
import random
import numpy as np 
import unittest
from RExample import *
from Latex import *
from python2R import dict2assignments, matrix2matrix
from subprocess import call
from coordArrays import tuplelist
from tools import who_am_i

class TestFrameworkFunctionality(unittest.TestCase):
   
    def test_SympyMat2RMat(self):
        a=Symbol("a")
        # we chose a Matrix with an expression that has to be translated
        # from a**2->a^2 
        self.assertEqual(matrix2matrix(Matrix(2,1,[a,a**2])),"matrix(byrow=TRUE,nrow=2,c(a,a^2))")
        # test the other format with lists
        self.assertEqual(matrix2matrix(Matrix([[a,a,a**3],[a,a,a**3]])),"matrix(byrow=TRUE,nrow=2,c(a,a,a^3,a,a,a^3))")
        # test the other format with zeros
        self.assertEqual(matrix2matrix(Matrix([[0],[0]])),"matrix(byrow=TRUE,nrow=2,c(0,0))")
    def test_vector2assignments(self):
        C_S,C_B,varepsilon=symbols("C_S,C_B,varepsilon") 
        k_S, k_B, k_M =symbols("k_S k_B k_M")
        alphas={"1_to_2":varepsilon,"2_to_1":1}
        f1=k_S*C_B*C_S/(k_M+C_S)
        f2=k_B*C_B
        F=Matrix(2,1,[f1,f2])
        C=Matrix(2,1,[C_S,C_B])
        
        tPM=RExample(C,alphas,F,I,who_am_i())
        res=tPM.cvecAssignments()
        ref="C_S=C[1]\nC_B=C[2]"
        self.assertEqual(res,ref)
    def test_python2R(self):
        a,b=symbols("a b")
        dic={a:5,b:6}
        print(dict2assignments(dic))
    def test_failingSystemCommand(self):
        # now we test a Model that can not run in R since one crucial parameter is missing
        C_0,t =symbols("C_0, t") 
        C_S,C_B,varepsilon,t_start,t_end,tn=symbols("C_S,C_B,varepsilon,t_start,t_end,tn") 
        V_S, mu_B, K_S =symbols("V_S mu_B K_S")
        F_npp=symbols("F_npp")
        alphas={"1_to_2":1,"2_to_1":varepsilon}
        F=Matrix(2,1,[
            mu_B*C_B,
            V_S*C_B*C_S/(K_S+C_S)
            ])
        C=Matrix(2,1,[C_B,C_S])
        I=Matrix(2,1,[0,F_npp])
        
        M=RExample(C,alphas,F,I,who_am_i())

        dic={ 
        	F_npp:0.01,
            V_S:0.000018,
        	mu_B:0.007,
        	K_S:1.0,
            varepsilon:0.5,
            # the following always have to be present
            C_0:Matrix(2,1,[ 972, 304 ]),
            t_start:0,
            t_end:2000,
            #tn:100 should cause an error
        	}
        M.addParameterSet(dic)
        with self.assertRaises(RcodeException):
            M.writeSoilRModel()
    
    def test_positiveEigenvalueOfJacobian(self):
        # this nonlinear model produces negative decay constants 
        # for C1<a or C2 <b
        # sympy is not able to detect this for the symbolic version
        # but the Rcode checks the numerical solution

        C_0,t =symbols("C_0, t")
        C1,C2=symbols("C1,C2",real=True,nonnegative=True)
        i1,i2=symbols("i1,i2",real=True,nonnegative=True)
        epsilon1,epsilon2=symbols("epsilon1,epsilon2",real=True,nonnegative=True)
        t_start,t_end,tn=symbols("t_start,t_end,tn") 
        #a1=i1
        #a2=i2
        C=Matrix(1,1,[C1])
        F=Matrix([C1*(sin(C1)+1+epsilon1)])
        I=Matrix(1,1,[i1])
        pprint(I)
        alphas={}
        Model=RExample(C,alphas,F,I,"positiveEigenvalueOfJacobian")
        #Css=sol[1]
        #M.suggestFixedPoint( {C1:0.876085889442093,C2:0.876085889442093})
        
        ranges=[[0,100]]
        vectors=[ linspace(l[0],l[1],10) for l in ranges] 
        startValues=tuplelist(vectors)
        dic={ 
            epsilon1:0.4,
            epsilon2:0.1,
            i1:10,
            i2:10,
            # the following always have to be present
            C_0:Matrix(1,1,[1]),
            t_start:0,
            t_end:10,
            tn:100 ,
            # the following are needed if a numerical search for fixedpoints is to be conducted
            "startValuesForFixedPointSearch":startValues,   
            "plotRanges":ranges
        	}

        Model.addParameterSet(dic)
        Model.computeNumericalFixedPoints()
        Model.writeSoilRModel()
        Model.latexSummary()
    def test_SOMCreationDetection(self):
        # this nonlinear model produces negative decay constants 
        # for C1<a or C2 <b
        # sympy is not able to detect this for the symbolic version
        # but the Rcode checks the numerical solution
        C_0,t =symbols("C_0, t")
        C1,C2=symbols("C1,C2",real=True,nonnegative=True)
        a,b=symbols("a,b",real=True,positive=True)
        t_start,t_end,tn=symbols("t_start,t_end,tn") 
        C=Matrix(2,1,[C1,C2])
        F=Matrix([C1*(C1-a),C2*(C2-b)])
        I=Matrix(2,1,[0,0])
        alphas={}
        M=RExample(C,alphas,F,I,"SOMCreation")
        #sol=solve(F,C)
        #Css=sol[1]
        M.suggestFixedPoint( {C1:a,C2:b})
        
        dic={ 
            a:5,
            b:5,
            # the following always have to be present
            C_0:Matrix(2,1,[1,1]),
            t_start:0,
            t_end:10,
            tn:100 
        	}

        M.addParameterSet(dic)
        M.latexSummary()
        with self.assertRaises(RcodeException):
            M.writeSoilRModel()
    def test_writeToSubDirDecorator(self):
        # This model is releates to TwopMMmodel
        # and Manzoni
        # C pools and startvector
        C_S, C_B, C_0=symbols("C_S, C_B, C_0",real=True) 
        C=Matrix(2,1,[C_S,C_B]) 
        
        #respired carbon fraction
        r=symbols("r") 
        
        # a scalar representing the Activity factor;
        # i.e. a temperature and moisture modifier (unitless)
        A_f=symbols("A_f") 

        # the michaelis konstant 
        K_m=symbols("K_m") 
        
        #decay rates for (S)oil and (B)acteria
        k_S, k_B =symbols("k_S, k_B") 
        
        #input flux to pool S
        ADD=symbols("ADD") 
        # the alphas have to be a function of C and t (at most)
        alphas={"1_to_2":1-r,"2_to_1":1}

        F=Matrix(2,1,[
            k_S*C_B*(C_S/(K_m+C_S)),
            k_B*C_B
            ])
        I=Matrix(2,1,[ADD,0])
        
        tPM=RExample(C,alphas,F,I,who_am_i())

        #times and time steps for the numeric modelling 
        t,t_start,t_end,tn=symbols("t,t_start,t_end,tn",real=True) 
        # the following parameter set should produce a negative fixed point
        # since C_S*=k_BK_M/(k_s(1-r)-kb) and k_s(1-r)<kb 
        dic={ 
        	ADD:3.3,
            k_S:2e-3,
        	k_B:2.3e-3,
        	K_m:1000,
        	K_m:900,
            r:0.6,
            t_start:0,
            t_end:1000,
            tn:2000,
            "C_Ranges":[np.linspace(0,13000,num=500),np.linspace(0,13000,num=500)],
            C_0:Matrix(2,1,[ 100, 10 ])
        	}
        tPM.addParameterSet(dic)
        tPM.suggestFixedPoint({
            C_S:k_B*K_m/(k_S*(1-r)-k_B),
            C_B:ADD*(1-r)/(k_B*r)
            })
        tPM.latexPhasePlanePlot()
        #tPM.writeSoilRModel()
        #tPM.latexSummary()
    def test_negativeRespirationDetection(self):
        # now we test a Model that can not run in R since one crucial parameter is missing
        C_0,t =symbols("C_0, t") 
        C_S,C_B,varepsilon,t_start,t_end,tn=symbols("C_S,C_B,varepsilon,t_start,t_end,tn") 
        V_S, mu_B, K_S =symbols("V_S mu_B K_S")
        F_npp=symbols("F_npp")
        alphas={"1_to_2":1,"2_to_1":varepsilon}
        F=Matrix(2,1,[
            mu_B*C_B,
            V_S*C_B*C_S/(K_S+C_S)
            ])
        C=Matrix(2,1,[C_B,C_S])
        I=Matrix(2,1,[0,F_npp])
        
        M=RExample(C,alphas,F,I,who_am_i())

        dic={ 
        	F_npp:0.01,
            V_S:0.000018,
        	mu_B:0.007,
        	K_S:1.0,
            # negative transfer should lead to a 
            varepsilon:-0.5,
            # the following always have to be present
            C_0:Matrix(2,1,[ 972, 304 ]),
            t_start:0,
            t_end:2000,
            tn:100 
        	}
        M.addParameterSet(dic)
        with self.assertRaises(NegativeRespiration):
            M.latexSummary()
    def test_oscillatingLinearModel_1(self):
        # we provoke a pair of complex conjugate eigenvalues with real part a and imagenary part b,-b respectively
        # by constructing an appropriate matrix with a on the diagonal and b,-b as off diagonal terms
        C_0,t =symbols("C_0, t")
        C1,C2=symbols("C1,C2",real=True,nonnegative=True)
        a,b=symbols("a,b",real=True,positive=True)
        i1,i2=symbols("i1,i2",real=True,positive=True)
        t_start,t_end,tn=symbols("t_start,t_end,tn") 
        C=Matrix(2,1,[C1,C2])
        #F=Matrix([C1*a,C2*a])
        F=Matrix([C1*a+b*C2,C2*a-b*C1])
        I=Matrix(2,1,[i1,i2])
        alphas={}
        M=RExample(C,alphas,F,I,who_am_i())
        
        dic={ 
            a:1,
            b:5,
            i1:1,
            i2:1,
            # the following always have to be present
            C_0:Matrix(2,1,[1,1]),
            t_start:0,
            t_end:10,
            tn:100 
        	}

        M.addParameterSet(dic)
        M.latexSummary()
        with self.assertRaises(RcodeException):
            M.writeSoilRModel()
    def test_oscillatingLinearModel_2(self):
        # we provoke a pair of complex conjugate eigenvalues with real part a and imagenary part b,-b respectively
        # by constructing an appropriate matrix with a on the diagonal and b,-b as off diagonal terms
        C_0,t =symbols("C_0, t")
        C1,C2=symbols("C1,C2",real=True,nonnegative=True)
        a,b=symbols("a,b",real=True,positive=True)
        i1,i2=symbols("i1,i2",real=True,positive=True)
        t_start,t_end,tn=symbols("t_start,t_end,tn") 
        C=Matrix(2,1,[C1,C2])
        F=Matrix([C1*a,C2*a])
        #F=Matrix([C1*a+b*C2,C2*a-b*C1])
        I=Matrix(2,1,[i1,i2])
        #alphas={}
        alphas={"1_to_2":b/a,"2_to_1":-b/a}
        M=RExample(C,alphas,F,I,who_am_i())
        
        dic={ 
            a:5,
            b:1,
            i1:1,
            i2:1,
            # the following always have to be present
            C_0:Matrix(2,1,[1,1]),
            t_start:0,
            t_end:10,
            tn:100 
        	}

        M.addParameterSet(dic)
        with self.assertRaises(NegativeRespiration):
            M.latexSummary()
        M.writeSoilRModel()

    def test_timeDependentNotConverging(self):
        # by creating a time dependent input we easily create a model that never converges
        # although it is very sensible and good natured
        C_0,t =symbols("C_0, t")
        C1,C2=symbols("C1,C2",real=True,nonnegative=True)
        a,b=symbols("a,b",real=True,positive=True)
        i1,i2=symbols("i1,i2",real=True,positive=True)
        t_start,t_end,tn=symbols("t_start,t_end,tn") 
        C=Matrix(2,1,[C1,C2])
        F=Matrix([C1*a,C2*a])
        I=Matrix(2,1,[i1*sin(t),i2*sin(t)])
        alphas={}
        M=RExample(C,alphas,F,I,who_am_i())
        
        dic={ 
            a:5,
            b:1,
            i1:1,
            i2:1,
            # the following always have to be present
            C_0:Matrix(2,1,[1,1]),
            t_start:0,
            t_end:100,
            tn:1000 
        	}

        M.addParameterSet(dic)
        M.latexSummary()
        with self.assertRaises(RcodeException):
            M.writeSoilRModel()

    def test_linearUnstableWithoutInertPool(self):
        C_0,t =symbols("C_0, t")
        C1,C2=symbols("C1,C2",real=True,nonnegative=True)
        a,b=symbols("a,b",real=True,positive=True)
        i1,i2=symbols("i1,i2",real=True,positive=True)
        t_start,t_end,tn=symbols("t_start,t_end,tn") 
        C=Matrix(2,1,[C1,C2])
        #F=Matrix([C1*a,C2*a])
        F=Matrix([C1*a+b*C2,C2*a-b*C1])
        I=Matrix(2,1,[i1,i2])
        alphas={}
        M=RExample(C,alphas,F,I,who_am_i())
        
        dic={ 
            a:1, #this leads to permanent oscillation without decay
            b:5,
            i1:1,
            i2:1,
            # the following always have to be present
            C_0:Matrix(2,1,[1,1]),
            t_start:0,
            t_end:10,
            tn:100 
        	}

        M.addParameterSet(dic)
        M.latexSummary()
        with self.assertRaises(RcodeException):
            M.writeSoilRModel()
        

if __name__ == '__main__':
    res=unittest.main()
    if len(res.errors) + len(res.failures) > 0:
        sys.exit(1)
