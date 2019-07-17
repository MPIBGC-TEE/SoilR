#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
#options(warn=-1)
#source("prolog.R")
require("RUnit")
require("pkgload")
source("../testhelpers.R")
pkgload::load_all('../../../')
alltests <- defineTestSuite(
   name="selectedCorrado",
   dirs=c("."),
   testFileRegexp = "^runit.corrado.R$",
   #testFuncRegexp ="^test.ConstLinDecompOpFromNamedFluxes$",
   #testFuncRegexp ="^test.ConstLinDecompOp$",
   #testFuncRegexp ="^test.phi_mn_with_initialValues|test.corrados_next_function",
   testFuncRegexp ="test.corrados_next_function",
   rngKind = "Marsaglia-Multicarry",
   rngNormalKind = "Kinderman-Ramage"
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
