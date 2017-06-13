#!/usr/bin/python3
# vim: set expandtab ts=4
import unittest 
from modelSet import *
from punitExample import *
from negativeCtest import *
from mpi4py import MPI
from failingModels import *

comm = MPI.COMM_WORLD
size = comm.Get_size()
rank = comm.Get_rank()

if rank == 0:
    def suite():
        s=unittest.TestSuite()

        #s.addTest(Test(""))
        s.addTest(TestSequenceFunctions("test_Manzoni"))
        #s.addTest(TestFrameworkFunctionality("test_writeToSubDirDecorator"))
        #s.addTest(TestFrameworkFunctionality("test_linearUnstableWithoutInertPool"))
        #s.addTest(TestFrameworkFunctionality("test_oscillatingLinearModel_1"))
        #s.addTest(TestFrameworkFunctionality("test_oscillatingLinearModel_2"))
        #s.addTest(TestSequenceFunctions("test_positiveEigenvalueOfJacobian"))
        #s.addTest(TestUnusualExamples("test_negativeC"))
        #s.addTest(TestUnusualExamples("test_negativeC2"))
        #s.addTest(TestUnusualExamples("test_negativeC32"))
        #s.addTest(TestSequenceFunctions("test_Wang2pools"))
        #s.addTest(TestSequenceFunctions("test_MEND_NUM"))
        #s.addTest(TestSequenceFunctions("test_MEND"))
        #s.addTest(TestSequenceFunctions("test_SW"))
        #s.addTest(TestSequenceFunctions("test_AWB"))
        #s.addTest(TestSequenceFunctions("test_Yasso"))
        #s.addTest(TestSequenceFunctions("test_Century"))
        #s.addTest(TestSequenceFunctions("test_RothC"))
        #s.addTest(TestSequenceFunctions("test_ICBM"))
        #s.addTest(TestSequenceFunctions("test_SW_RMM"))
        #s.addTest(TestSequenceFunctions("test_SW_RMM_S"))

        return(s)
    
    if  __name__ == '__main__':
        unittest.TextTestRunner(verbosity=2).run(suite())
