#!/usr/bin/env Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
require("RUnit")
require("SoilR")
package_tests <- defineTestSuite(
   name="package_Tests",
   #dirs = c(
   # file.path(path.package(package="SoilR"), "tests","protected") ,
   # file.path(path.package(package="SoilR"), "tests","requireSoilR")
   # )
   #,
   dirs=c("protected","requireSoilR"),
   #dirs=file.path(path.package(package="SoilR"), "tests","protected"),
   testFileRegexp = "^runit.+\\.[rR]",
   testFuncRegexp = "^test.+",
   rngKind = "Marsaglia-Multicarry",
   rngNormalKind = "Kinderman-Ramage"
)

#testResult <- runTestSuite(testSuites=list(source_tests,package_tests))
testResult <- runTestSuite(testSuites=list(package_tests))
printTextProtocol(testResult)

#produce exitstatus ne 0 for buildbot to notice
ef=getErrors(testResult)
n=ef$nErr+ef$nFail
if (n>0) {stop(1)}
