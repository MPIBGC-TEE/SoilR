#!/usr/bin/python3
# vim:set ff=unix expandtab ts=4 sw=4:
import sys
from sympy import symbols, Symbol, Matrix
from sympy.functions import sin, cos, Abs
from sympy.matrices import MatrixBase 
from sympy.abc import x
import random
import unittest
import numpy as np
from concurrencytest import ConcurrentTestSuite, fork_for_tests
from RExample import *
from Latex import *
import Rcode
from python2R import dict2assignments, matrix2matrix
from tools import who_am_i
from datetime import datetime

class TestSequenceFunctions(unittest.TestCase):
    def run(self,*args):
        print("####################################################################")
        print(self.id)
        print("####################################################################")
        before=datetime.now()
        super().run(*args)
        after=datetime.now()
        print("time consumed=",after-before)
        print("####################################################################")

    def test_Manzoni(self):
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
            k_S:4e-3,
        	k_B:2.3e-3,
        	K_m:1000,
        	K_m:900,
            r:0.6,
            t_start:0,
            t_end:1000,
            tn:2000,
            "C_Ranges":[np.linspace(0,20,num=50),np.linspace(0,20,num=50)],
            C_0:Matrix(2,1,[ 100, 10 ])
        	}
        tPM.addParameterSet(dic)
        dic={ 
        	ADD:3.3,
            k_S:8e-3,
        	k_B:2.3e-3,
        	K_m:1000,
            r:0.6,
            t_start:0,
            t_end:1000,
            tn:2000,
            "C_Ranges":[np.linspace(0,20,num=50),np.linspace(0,20,num=50)],
            C_0:Matrix(2,1,[ 100, 10 ])
        	}
        tPM.addParameterSet(dic)
        tPM.suggestFixedPoint({
            C_S:k_B*K_m/(k_S*(1-r)-k_B),
            C_B:ADD*(1-r)/(k_B*r)
            })
        #tPM.latexAttractionLandscape()
        #tPM.writeSoilRModel()
        tPM.latexSummary()

    def test_SW_RMM(self):
        C_0,t =symbols("C_0, t",real=True)
        t_start,t_end,tn=symbols("t_start,t_end,tn")
        S, D, M, E=symbols("S, D, M, E")
        C=Matrix(4,1,[S,D,M,E])

        K_e=Symbol("K_e", real=True) # Fraction of DOC used for enzyme production.
        K_d=Symbol("K_d", real=True) # Decay constant.
        K_t=Symbol("K_t", real=True) # proportion of microbial biomass that dies in each time interval.
        K_r=Symbol("K_r", real=True) # proportion of dead microbial biomass that is available for microbial use.
        K_m=Symbol("K_m", real=True) # microbial maintenance rate.
        K_l=Symbol("K_l", real=True) # decay constant for exoenzymes. 
        K_es=Symbol("K_es", real=True) # half saturation constant for enzymes on substrate. 

        alphas={"1_to_2":1, "2_to_3":1-K_e, "2_to_4":K_e, "3_to_2":(K_t*K_r)/K_m}

        I=Matrix(4,1,[0,0,0,0])
        F=Matrix(4,1,[K_d*E/(K_es + E), D, K_m*M, K_l*E])
        
        Mod=RExample(C,alphas,F,I,who_am_i())
        Cdot=I+Mod.getOperator()
        solutions=solve(Cdot,[D, M, E])
        pprint(solutions)
        Mod.suggestFixedPoint( {
            #S   arbitrary since not present in the operator?
            D:simplify(solutions[D]),
            M:simplify(solutions[M]),
            E:simplify(solutions[E]),
        })
        
        Mod.addParameterSet({
            K_e:0.005, 
            K_d:1.0, 
            K_t:0.012, 
            K_r:0.85, 
            K_m:0.22, 
            K_l:0.005,
	    K_es:0.3,
            t_start:0,
            t_end:2e5,
            tn:2e4,
            C_0:Matrix(4,1,[.1,.1,.1,.1])
        })

        Mod.latexSummary()
        Mod.writeSoilRModel()

    def test_SW_RMM_S(self):
        C_0,t =symbols("C_0, t",real=True)
        t_start,t_end,tn=symbols("t_start,t_end,tn")
        S, D, M, E=symbols("S, D, M, E")
        C=Matrix(4,1,[S,D,M,E])

        K_e=Symbol("K_e", real=True) # Fraction of DOC used for enzyme production.
        K_d=Symbol("K_d", real=True) # Decay constant.
        K_t=Symbol("K_t", real=True) # proportion of microbial biomass that dies in each time interval.
        K_r=Symbol("K_r", real=True) # proportion of dead microbial biomass that is available for microbial use.
        K_m=Symbol("K_m", real=True) # microbial maintenance rate.
        K_l=Symbol("K_l", real=True) # decay constant for exoenzymes. 
        K_es=Symbol("K_es", real=True) # half saturation constant for enzymes on substrate. 

        alphas={"1_to_2":1, "2_to_3":1-K_e, "2_to_4":K_e, "3_to_2":(K_t*K_r)/K_m}

        I=Matrix(4,1,[0,0,0,0])
        F=Matrix(4,1,[K_d*S*E/(K_es + E), D, K_m*M, K_l*E])
        
        Mod=RExample(C,alphas,F,I,who_am_i())
        Cdot=I+Mod.getOperator()
        solutions=solve(Cdot,[D, M, E])
        pprint(solutions)
        Mod.suggestFixedPoint( {
            S:simplify(solutions[S]), 
            D:simplify(solutions[D]),
            M:simplify(solutions[M]),
            E:simplify(solutions[E]),
        })
        
        Mod.addParameterSet({
            K_e:0.005, 
            K_d:0.001, 
            K_t:0.012, 
            K_r:0.85, 
            K_m:0.22, 
            K_l:0.005,
	    K_es:0.3,
            t_start:0,
            t_end:2e5,
            tn:2e4,
            C_0:Matrix(4,1,[.1,.1,.1,.1])
        })

        Mod.latexSummary()
        Mod.writeSoilRModel()
   
    def test_SW(self):
        C_0,t =symbols("C_0, t",real=True)
        t_start,t_end,tn=symbols("t_start,t_end,tn")
        S, D, M, E=symbols("S, D, M, E")
        C=Matrix(4,1,[S,D,M,E])

        K_e=Symbol("K_e", real=True) # Fraction of DOC used for enzyme production.
        K_d=Symbol("K_d", real=True) # Decay constant.
        K_t=Symbol("K_t", real=True) # proportion of microbial biomass that dies in each time interval.
        K_r=Symbol("K_r", real=True) # proportion of dead microbial biomass that is available for microbial use.
        K_m=Symbol("K_m", real=True) # microbial maintenance rate.
        K_l=Symbol("K_l", real=True) # decay constant for exoenzymes. 

        alphas={"1_to_2":1, "2_to_3":1-K_e, "2_to_4":K_e, "3_to_2":(K_t*K_r)/K_m}

        I=Matrix(4,1,[0,0,0,0])
        F=Matrix(4,1,[K_d*E, D, K_m*M, K_l*E])
        
        Mod=RExample(C,alphas,F,I,who_am_i())
        Cdot=I+Mod.getOperator()
        solutions=solve(Cdot,[D, M, E])
        pprint(solutions)
        Mod.suggestFixedPoint( {
            #S   arbitrary since not present in the operator?
            D:simplify(solutions[D]),
            M:simplify(solutions[M]),
            E:simplify(solutions[E]),
        })
        
        Mod.addParameterSet({
            K_e:0.005, 
            K_d:1.0, 
            K_t:0.012, 
            K_r:0.85, 
            K_m:0.22, 
            K_l:0.005,
            t_start:0,
            t_end:2e5,
            tn:2e4,
            C_0:Matrix(4,1,[.1,.1,.1,.1])
        })

        Mod.latexSummary()
        Mod.writeSoilRModel()

    def test_Century(self):
        C_0,t =symbols("C_0, t",real=True) 
        t_start,t_end,tn=symbols("t_start,t_end,tn")
        C_1, C_2, C_3, C_4, C_5=symbols("C_1, C_2, C_3, C_4, C_5")
        C=Matrix(5,1,[C_1,C_2,C_3,C_4,C_5])

        k_1=Symbol("k_1",nonnegative=True,real=True) # Decomposition rate of the structural litter.
        k_2=Symbol("k_2",nonnegative=True,real=True) # Decomposition rate of the metabolic littter.
        k_3=Symbol("k_3",nonnegative=True,real=True) # Decomposition rate of the Active pool.
        k_4=Symbol("k_4",nonnegative=True,real=True) # Decomposition rate of the Slow pool.
        k_5=Symbol("k_5",nonnegative=True,real=True) # Decomosition rate of the Passive pool.
        In=Symbol("In",nonnegative=True,real=True) # Carbon inputs.
        LN=Symbol("LN",nonnegative=True,real=True) # Lignin to nitrogen ratio.
        Ls=Symbol("Ls",nonnegative=True,real=True) # Fraction of structural material that is lignin.
        c=Symbol("c",nonnegative=True,real=True) # Proportion of clay in mineral soil.
        s=Symbol("s",nonnegative=True,real=True) # Proportion of silt in mineral soil.
        alpha31=Symbol("alpha31",real=True) # Transfer coefficient.
        alpha32=Symbol("alpha32",real=True) # Transfer coefficient.
        alpha34=Symbol("alpha34",real=True) # Transfer coefficient.
        alpha35=Symbol("alpha35",real=True) # Transfer coefficient.
        alpha41=Symbol("alpha41",real=True) # Transfer coefficient.
        alpha53=Symbol("alpha53",real=True) # Transfer coefficient.
        alpha54=Symbol("alpha54",real=True) # Transfer coefficient.

        Fm=0.85-0.18*LN
        Fs=1-Fm
        Tx=c+s
        fTx=1-0.75*Tx
        Es=0.85-0.68*Tx
        alpha43=1-Es-alpha53

        alphas={"1_to_3":alpha31,
               "2_to_3":alpha32,
               "4_to_3":alpha34,
               "5_to_3":alpha35,
               "1_to_4":alpha41,
               "3_to_5":alpha53,
               "3_to_4":alpha43,
               "4_to_5":alpha54}

        I=Matrix(5,1,[In*Fm,In*Fs,0,0,0])
        F=Matrix(5,1,[k_1*C_1*exp(-3.0*Ls),
                      k_2*C_2,
                      k_3*C_3*fTx,
                      k_4*C_4,
                      k_5*C_5])

        
        Mod=RExample(C,alphas,F,I,who_am_i())

        Cdot=I+Mod.getOperator()
        solutions=solve(Cdot,[C[i] for i in range(0,len(C))])
        pprint(solutions)
        Mod.suggestFixedPoint( {C[i]:solutions[C[i]] for i in range(0,len(C))})

        Mod.addParameterSet({k_1:0.094, k_2:0.35, k_3:0.14, k_4:0.0038, k_5:0.00013,
                           c:0.2, s:0.45, LN:0.5,Ls:0.1, In:0.1, alpha31:0.45,
                           alpha32:0.55, alpha34:0.42, alpha35:0.45,alpha41:0.3, alpha53:0.004,
                           alpha54:0.03,
                           t_start:0,
                           t_end:2e5,
                           tn:2e4,
                           C_0:Matrix(5,1,[.1,.1,.1,.1,.1])
                           })
        Mod.latexSummary()
        Mod.writeSoilRModel()
if __name__ == '__main__':
    suite = unittest.TestLoader().loadTestsFromTestCase(TestSequenceFunctions)
    # Run same tests across 16 processes
    #concurrent_suite = ConcurrentTestSuite(suite, fork_for_tests(16))
    concurrent_suite = ConcurrentTestSuite(suite, fork_for_tests(1))
    runner = unittest.TextTestRunner()
    runner.run(concurrent_suite)

    #unittest.main()
 
