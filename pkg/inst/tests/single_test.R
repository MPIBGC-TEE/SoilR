#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
source("prolog.R")
alltests <- defineTestSuite(
   name="allTests",
   #dirs=c("."),
   dirs=c(".","protected","automatic"),
   #testFileRegexp = "^runit.NonlinearAtmosphericModel.R",
   #testFileRegexp = "^runit.BoundFc.R",
   #testFileRegexp = "^runit.TimeMap.R",
   #testFileRegexp = "^runit.BoundLinDecomOp.R",
   #testFileRegexp = "^runit.Model.R",
   #testFileRegexp = "^runit.formatConversion",
   #testFileRegexp = "^runit.check.pass.R",
   #testFileRegexp = "^runit.ThreepFeedback_MCSim.R",
   testFileRegexp = "^runit.*.R",
   testFuncRegexp = "^test.TwopSerial_linear_vs_nonlinear",
   #testFuncRegexp = "^test.+",
   #testFuncRegexp = "^test.correctnessOfModel.impossibleCoefficients",
   #testFuncRegexp = "^test.correctnessOfModel.correctModel",
   rngKind = "Marsaglia-Multicarry",
   rngNormalKind = "Kinderman-Ramage"
)

testResult <- runTestSuite(alltests)#,verbose=0)
print(testResult)
#printTextProtocol(testResult)
#produce exitstatus ne 0 for buildbot to notice
ef=getErrors(testResult)
print(ef)
n=ef$nErr+ef$nFail
if (n>0) {stop(1)}
