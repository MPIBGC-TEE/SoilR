#!/bin/bash
set -ev
cd ../pkg/inst/tests/sall.R
Rscript run_all_test.R
echo "Yeah"
