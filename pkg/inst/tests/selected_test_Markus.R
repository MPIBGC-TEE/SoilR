#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
source("prolog.R")
#require(RUnit)
alltests <- defineTestSuite(
   name="allTests",
   #dirs=c("."),
   dirs=c(".","protected","automatic"),
   #dirs=c("automatic"),
   #testFileRegexp = "^runit.NonlinearAtmosphericModel.R",
   #testFileRegexp = "^runit.BoundFc.R",
   #testFileRegexp = "^runit.TimeMap.R",
   #testFileRegexp = "^runit.BoundLinDecomOp.R",
   #testFileRegexp = "^runit.Model.R",
   #testFileRegexp = "^runit.formatConversion",
   #testFileRegexp = "^runit.check.pass.single.R",
   #testFileRegexp = "^runit.check.pass.R",
   #testFileRegexp = "^runit.Conversion.R",
   testFileRegexp = "^runit.tensorList.R",
   #testFileRegexp = "^runit.ThreepFeedback_MCSim.R",
   #testFileRegexp = "^runit.*.R$",
   #testFuncRegexp = "^test.FourpSerial_1",
   #testFuncRegexp = "^test.SoilRPaper1",
   #testFuncRegexp = "^test.TwopFeedback",
   testFuncRegexp = "^test.+",
   #testFuncRegexp = "^test.TimeMapFrom1Ddataframe",
   #testFuncRegexp = "^test.TimeMapFromListOfVectorAndList",
   #testFuncRegexp = "^test.TimeMapFromListOfVectorAndArray",
   #testFuncRegexp = "^test.correctnessOfModel.impossibleCoefficients",
   #testFuncRegexp = "^test.correctnessOfModel.correctModel",
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
