#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
require('RUnit')
require('SoilR')
alltests <- defineTestSuite(
   name="selected_Packagetest_Markus",
   dirs=c(".","protected","automatic","requireSoilR"),
   #testFileRegexp = "^runit.TimeMap.R",
   #testFileRegexp = "runit.all_possible_GeneralDecompOp_arguments.R",
   testFileRegexp = "runit.all_possible_Model_arguments.R",
   testFuncRegexp = "^test.*",
   rngKind = "Marsaglia-Multicarry",
   rngNormalKind = "Kinderman-Ramage"
)
testResult <- runTestSuite(alltests)#,verbose=0)
#print(testResult)
printTextProtocol(testResult)
#produce exitstatus ne 0 for buildbot to notice
ef=getErrors(testResult)
print(ef)
n=ef$nErr+ef$nFail
if (n>0) {stop(1)}
