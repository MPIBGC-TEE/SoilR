#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
#source("prolog.R")
tfr  <- "^runit\\..*\\.R"
require("parallel")
require("RUnit")
require("deSolve")
require("devtools")
#devtools::install('~/SoilR-exp/pkg')
require("SoilR")

source("../testhelpers.R")
#fl <- list.files(pattern=tfr)
#for (fn in fl){
#  print(fn)
#  source(fn)
#}
alltests <- defineTestSuite(
   name="suite for manual testing with the SoilR package loaded",
   dirs=c("."),
   testFileRegexp = tfr,
   "test.corrado"
   #"test.ThreepSerialFeedback_linear_vs_nonlinear"
   #"test.ConstLinDecompOp"
)

testResult <- runTestSuite(alltests)
printTextProtocol(testResult,separateFailureList=TRUE)
#produce exitstatus ne 0 for buildbot to notice
ef=getErrors(testResult)
#print(ef)
n=ef$nErr+ef$nFail
if (n>0) {stop(1)}
