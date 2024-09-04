#!/bin/bash
# Use second argument for options to R CMD check

echo "Testing $1 using R CMD check internal functions"
R CMD check $1 $2
