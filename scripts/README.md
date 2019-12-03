The scripts try to have descriptive names.
The real work is done by functions in helperFunctions.R
which tries to avoid duplication.

To update the rd files:
"./build_and_check_Rd.R"

To check the package as cran:
"./testPkg.R" (This will not build the updated docs, so run build_and_check_Rd.R first"

You also source the helperFunction.R and call the functions ad libitum.
