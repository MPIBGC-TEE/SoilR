#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
#options(warn=-1)
require("RUnit")
require("pkgload")
source("../testhelpers.R")
pkgload::load_all('../../../')
alltests <- defineTestSuite(
   name="selectedMarkus"
   ,dirs=c(".")
   ,testFileRegexp = "^runit.Century.R$"
   #,testFuncRegexp ="^test.function_by_PoolIndex$"
   ,rngKind = "Marsaglia-Multicarry"
   ,rngNormalKind = "Kinderman-Ramage"
)

#testResult <- runTestSuite(alltests,verbose=0)
testResult <- runTestSuite(alltests)
#print(testResult)
printTextProtocol(testResult)
#produce exitstatus ne 0 for buildbot to notice
ef=getErrors(testResult)
print(ef)
n=ef$nErr+ef$nFail
if (n>0) {stop(1)}
