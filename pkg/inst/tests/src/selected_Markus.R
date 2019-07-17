#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
#options(warn=-1)
#source("prolog.R")
require("RUnit")
require("pkgload")
source("../testhelpers.R")
pkgload::load_all('../../../')
alltests <- defineTestSuite(
   name="selectedMarkus",
   dirs=c("."),
   #testFileRegexp = "^runit.FluxRateConstructors.R$|^runit.FluxConstructors.R$|^runit.PoolTopology.R$",
   #testFileRegexp = "^runit.PoolTopology.R$",
   #testFileRegexp = "^runit.FluxRateConstructors.R$",
   #testFileRegexp = "^runit.FluxConstructors.R$",
   #testFileRegexp = "^runit.NonlinearOperators.R$",
   
   #testFileRegexp = "^runit.ConstLinDecompOp.R$$",
   testFileRegexp = "^runit.NonLinDecompOp_with_linear_fluxes.R$",
   #testFuncRegexp ="^test.ConstLinDecompOpFromNamedFluxes$",
   #testFuncRegexp ="^test.ConstLinDecompOp$",
   testFuncRegexp ="^test.NonLinDecompOp_with_linear_fluxes_by_Index$",
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
