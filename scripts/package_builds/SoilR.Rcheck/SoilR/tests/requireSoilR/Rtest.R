#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
#source("prolog.R")
tfr  <- "^runit\\..*\\.R"
pkgDir<-'../../../'
if (is.element('SoilR',installed.packages())){
  devtools::uninstall(pkgDir)
}

devtools::install(pkgDir,quick=TRUE) # unfortunately does not stop on error therefore we uninstall first
require("parallel")
require("RUnit")
require("deSolve")
#require("debugHelpers")
require("devtools")
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
   
   #testFuncRegexp = "test.Ident"
   #"^test.FourpSerial_1"
   #"test.TwopParallel_ZeroInput"
   #"^test.TwopFeedback"
   #"^test.TimeMapInterface"
   #"^test.LowVerticalRatesPaper" 
   #"^test.check.pass"
   #"test.ModelInit"
   #"ptest.ModelOperators"
   #"test.ParallelModel"
   #"test.TwopSerial_MCSim"
   "test.NonlinearOperators"
   #"test.TwopSerial_linear_vs_nonlinear"
   #"test.SoilRPaper1"
   #"test.FourpSerial_1"
   #"test.BoundFc"
   #"test.ThreepFeedbackModel14|test.ThreepParallelModel14|test.ThreepSeriesModel14|test.TwopFeedbackModel14|test.TwopParallelModel14|test.TwopSeriesModel14"
   #"test.LowVerticalRatesPaper|test.ModelInit|test.SoilRPaper1"
   #"test.LowVerticalRatesPaper|test.SoilRPaper1"
   #"test.Deprecation"
   #"test.GaudinskiModel14"
   #"test.MC"
   #"test.ConstLinDecompOp"
)

testResult <- runTestSuite(alltests)
printTextProtocol(testResult,separateFailureList=TRUE)
#produce exitstatus ne 0 for buildbot to notice
ef=getErrors(testResult)
#print(ef)
n=ef$nErr+ef$nFail
if (n>0) {stop(1)}
