The scripts try to have descriptive names and are ment as abreviations for common tasks.
They refer to functions in helperFunctions.R.
The functions usually use devtools and are mainly a reminder about some necessarry flags.
All they can do, can equally well being achieved from a R session.

To update the rd files and the html files for the github documentation:
"./build_and_check_Rd.R"

To check the package as cran:
"./testPkg.R" (This will not build the updated docs, so run build_and_check_Rd.R first)"

You also source the helperFunction.R and call the functions ad libitum.
