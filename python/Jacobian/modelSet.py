#!/usr/bin/env python3
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

    def test_twoPMacrobial(self):
        # This model is releates to TwopMMmodel
        # C pools and startvector
        S, B, C_0=symbols("S, B, C_0") 
        C=Matrix(2,1,[S,B]) 
        
        #respired carbon fraction
        r=symbols("r") 
        
        # a scalar representing the Activity factor;
        # i.e. a temperature and moisture modifier (unitless)
        A_f=symbols("A_f") 

        # the michaelis konstant 
        K_m=symbols("K_m") 
        
        #decay rates for (S)oil and (B)acteria
        k_s, k_b =symbols("k_s, k_b") 
        
        #input flux to pool S
        ADD=symbols("ADD") 
        # the alphas have to be a function of C and t (at most)
        alphas={"1_to_2":1-r,"2_to_1":1}

        F=Matrix(2,1,[
            A_f*k_s*B*(S/(K_m+S)),
            k_b*B
            ])
        I=Matrix(2,1,[ADD,0])
        
        tPM=RExample(C,alphas,F,I,who_am_i())

        #times and time steps for the numeric modelling 
        t_start,t_end,tn=symbols("t_start,t_end,tn") 
        
        tPM.addParameterSet({ 
        	ADD:3.3,
        	A_f:1,
            k_s:0.000018,
        	k_b:0.007,
        	K_m:900,
            r:0.6,
            t_start:0,
            t_end:2000,
            tn:2000,
            C_0:Matrix(2,1,[ 100, 10 ])
        	})
        tPM.addParameterSet({ 
        	ADD:3.3,
        	A_f:3,
            k_s:0.000018,
        	k_b:0.0000001,
        	K_m:900,
            r:0.6,
            t_start:0,
            t_end:200000,
            tn:2000,
            C_0:Matrix(2,1,[ 972, 304 ])
        	})
        tPM.addParameterSet({ 
        	ADD:3.3,
        	A_f:3,
            k_s:0.000018,
        	k_b:0.0000001,
        	K_m:900,
            r:0.6,
            t_start:0,
            t_end:20000,
            tn:2000,
            C_0:Matrix(2,1,[ 4.18604651162791, 22000000.0 ])
        	})
        tPM.addParameterSet({
                ADD:345,
                A_f:1,
                k_s:42.82,
                k_b:4.38,
                K_m:269774.1,
                r:0.2,
            t_start:0,
            t_end:20000,
            tn:2000,
            C_0:Matrix(2,1,[ 4.18604651162791, 22000000.0 ])
                })
        tPM.suggestFixedPoint({
            S:k_b*K_m/(A_f*k_s*(1-r)-k_b),
            B:ADD*(1-r)/(k_b*r)
            })
        tPM.writeSoilRModel()
        tPM.latexSummary()


    def test_VegVero(self):
        # This model is releates to TwopMMmodel
        # C pools and startvector
        Xleaf, Xwood, Xroot, C_0=symbols("Xleaf, Xwood, Xroot, C_0") 
        C=Matrix(3,1,[Xleaf,Xwood,Xroot]) 
        
        # a scalar representing the Activity factor;
        # i.e. a temperature and moisture modifier (unitless)
        a1,a2,a3=symbols("a1,a2,a3") 
        b1,b2,b3=symbols("b1,b2,b3") 
        c1,c2,c3=symbols("c1,c2,c3") 
        u1,u2,u3=symbols("u1,u2,u3") 
        # the alphas have to be a function of C and t (at most)
        alphas={}

  #      F=Matrix(2,1,[
  #          A_f*k_s*B*(S/(K_m+S)),
  #          k_b*B
  #          ])
	
        F=Matrix(3,1,[
            -a1*c1*Xleaf,
            -a2*c2*Xwood,
            -a3*c3*Xroot
            ])
        I=Matrix(3,1,[
            b1*u1,
            b2*u2,
            b3*u3
            ])
        
        tPM=RExample(C,alphas,F,I,who_am_i())

        #times and time steps for the numeric modelling 
        t_start,t_end,tn=symbols("t_start,t_end,tn") 
        
        tPM.addParameterSet({ 
            a1:-1,
            a2:-1,
            a3:-1,
            b1:0.14,
            b2:0.26,
            b3:0.14,
            c1:0.00258,
            c2:0.0000586,
            c3:0.00239,
            u1:10,
            u2:10,
            u3:10,
            t_start:0,
            t_end:2000,
            tn:2000,
            C_0:Matrix(3,1,[ 100, 10,10 ])
        	})
        tPM.suggestFixedPoint({
            Xleaf:-b1*u1/(a1*c1),
            Xwood:-b2*u2/(a2*c2),
            Xroot:-b3*u3/(a3*c3)
            })
        tPM.writeSoilRModel()
        tPM.latexSummary()

    def test_Wang2pools(self):
        # This model is also releated to TwopMMmodel.R and Manzoni but slightly different and with other names
        C_0,t =symbols("C_0, t") # these always have to be present because the translation relies on them
        C_S,C_B,varepsilon,t_start,t_end,tn=symbols("C_S,C_B,varepsilon,t_start,t_end,tn") 
        V_S, mu_B, K_S =symbols("V_S mu_B K_S")
        F_npp=symbols("F_npp")
        # the alphas have to be a function of C and t (at most)
        alphas={"1_to_2":1,"2_to_1":varepsilon}
        F=Matrix(2,1,[
            mu_B*C_B,
            V_S*C_B*C_S/(K_S+C_S)
            ])
        C=Matrix(2,1,[C_B,C_S])
        I=Matrix(2,1,[0,F_npp])
        
        M=RExample(C,alphas,F,I,who_am_i())
        #pprint(tPM.jacobian().eigenvals())

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
            tn:100
        	}
        M.addParameterSet(dic)
        M.suggestFixedPoint({
            C_B:F_npp/(mu_B*(1.0/varepsilon-1)),
            C_S:K_S/(varepsilon*(V_S/mu_B)-1)
            })
        M.writeSoilRModel()
        M.latexSummary()
    def test_ICBM(self):
        C_0,t,t_start,t_end,tn =symbols("C_0, t,t_start,t_end,tn", real=True) # these always have to be present because the translation relies on them
        
        Y,O=symbols("Y,O")  #Young and old carbon pools
        h=Symbol("h")  #Humification coefficient. Fraction of the decomposed young C transferred to old pool
        k1=Symbol("k1")  #Decomposition rate of the young pool (yr-1).
        k2=Symbol("k2")  #Decomposition rate of the old pool (yr-1).
        r=Symbol("r") #External (climatic) effect on decomposition.  
        i=symbols("i") # Mean annual carbon input (kg C m-2 yr-1).

        # the alphas have to be a function of C and t (at most)
        alphas={"1_to_2":h}

        F=Matrix(2,1,[
            r*k1*Y,
            r*k2*O
            ])
        C=Matrix(2,1,[Y,O])
        I=Matrix(2,1,[i,0])
        
        M=RExample(C,alphas,F,I,who_am_i())
        
        Cdot=I+M.getOperator()
        print(Cdot.shape)
        solutions=solve(Cdot,Y,O)
        M.suggestFixedPoint({
            Y:simplify(solutions[Y]),
            #Y:i/(r*k1),
            O:simplify(solutions[O])
            #O:(h*i)/(r*k2)
            })

        dic={ #+N +straw 
        	h:0.125,
        	k1:0.8,
                k2:0.00605,
                r:1.0,
        	i:0.285,
        	# the following always have to be present
                 C_0:Matrix(2,1,[ 0.3, 4.11 ]),
            t_start:0,
            t_end:200,
            tn:400
        	}
        M.addParameterSet(dic)
        M.addParameterSet({ #Bare fallow
              h:0.13,
              k1:0.8,
              k2:0.00605,
              r:1.32,
              i:0,
              C_0:Matrix(2,1,[0.3, 3.96]),
              t_start:0,
              t_end:200,
              tn:400
        })
        M.addParameterSet({ # -N +Straw
              h:0.125,
              k1:0.8,
              k2:0.00605,
              r:1.22,
              i:0.248,
              C_0:Matrix(2,1,[0.3, 4.05]),
              t_start:0,
              t_end:200,
              tn:400
        })
        M.addParameterSet({ # -N -Straw
              h:0.125,
              k1:0.8,
              k2:0.00605,
              r:1.17,
              i:0.057,
              C_0:Matrix(2,1,[0.3, 3.99]),
              t_start:0,
              t_end:200,
              tn:400
        })
        M.addParameterSet({ # +N -Straw
              h:0.125,
              k1:0.8,
              k2:0.00605,
              r:1.07,
              i:0.091,
              C_0:Matrix(2,1,[0.3, 4.02]),
              t_start:0,
              t_end:200,
              tn:400
        })
        M.addParameterSet({ # Farmyard manure
              h:0.25,
              k1:0.8,
              k2:0.00605,
              r:1.1,
              i:0.272,
              C_0:Matrix(2,1,[0.3, 3.99]),
              t_start:0,
              t_end:200,
              tn:400
        })
        M.addParameterSet({ # Sewage sludge
              h:0.34,
              k1:0.8,
              k2:0.00605,
              r:0.97,
              i:0.296,
              C_0:Matrix(2,1,[0.3, 4.14]),
              t_start:0,
              t_end:200,
              tn:400
        })
        M.writeSoilRModel()
        M.latexSummary()
    def test_AWB(self):
        C_0,t =symbols("C_0, t",real=True)
        t_start,t_end,tn=symbols("t_start,t_end,tn")
        B, E, S, D=symbols("B, E, S, D")
        C=Matrix(4,1,[B,E,S,D])

        V_M=Symbol("V_M",real=True) # Maximum rate of uptake.
        V_m=Symbol("V_m",real=True) # maximum rate of SOM decomposition.
        r_B=Symbol("r_B",real=True) # Microbial death rate.
        r_E=Symbol("r_E",real=True) # Rate enzyme production.
        r_L=Symbol("r_L",real=True) # Rate enzyme loss.
        a_BS=Symbol("a_BS",real=True) # Fraction of dead microbial biomass to SOM.
        epsilon_0=Symbol("epsilon_0",real=True) # intercenpt of CUE function.
        epsilon_s=Symbol("epsilon_s",real=True) # Slope of CUE function.
        Km_0=Symbol("Km_0",real=True) # Intercept of half-sat. function.
        Km_s=Symbol("Km_s",real=True) # Slope of half-sat. function.
        Km_u0=Symbol("Km_u0",real=True) # Intercept of half-sat. function of uptake.
        Km_us=Symbol("Km_us",real=True) # Slope of half-sat. function of uptake.
        Ea=Symbol("Ea",real=True) # Activation energy.
        R=Symbol("R",real=True) # Gas constant.
        T=Symbol("T",real=True) # Temperature.
        I_S=Symbol("I_S",real=True) # Inputs to SOC (S) pool.
        I_D=Symbol("I_D",real=True) # Inputs to DOC (D)  pool.

        alphas={"1_to_2":r_E/(r_B+r_E),
                "1_to_3":(a_BS*r_B)/(r_B+r_E),
                "1_to_4":r_B*(1-a_BS)/(r_B+r_E),
                "4_to_1":epsilon_0+epsilon_s*T}

        I=Matrix(4,1,[0,0,I_S,I_D])
        F=Matrix(4,1,[(r_B+r_E)*B,
                       r_L*E,
                       V_m*exp(-Ea/(R*(T+273)))*(E*S)/((Km_s*T+Km_0)+S),
                       V_M*exp(-Ea/(R*(T+273)))*(B*D)/((Km_us*T+Km_u0)+D)])
        
        Mod=RExample(C,alphas,F,I,who_am_i())
        # computation does not finish after many hours
        # we should try to simplify the assumptions
        #Cdot=I+Mod.getOperator()
        #solutions=solve(Cdot,B, E, S, D)
        #Mod.suggestFixedPoint({
        #    B:simplify(solutions[B]),
        #    E:simplify(solutions[E]),
        #    S:simplify(solutions[S]),
        #    D:simplify(solutions[D]),
        #    })

        Mod.addParameterSet({V_M:100000000, V_m:100000000, r_B:0.0002, r_E:0.000005,
                           r_L:0.001, a_BS:0.5, epsilon_0:0.63, epsilon_s:0.016,
                           Km_0:500, Km_u0:0.1, Km_s:0.5, Km_us:0.1, Ea:47,
                           R:0.008314, T:20, I_S:0.005, I_D:0.005,
                           t_start:0,
                           t_end:2e5,
                           tn:2e4,
                           C_0:Matrix(4,1,[.1,.1,.1,.1])
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
 
 
 
 
