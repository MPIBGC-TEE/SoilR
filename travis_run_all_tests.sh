#!/bin/bash
set -ev
cd pkg/inst/tests
Rscript all.R
Rscript sall.R
echo "Yeah"
