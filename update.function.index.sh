#!/bin/sh
##########################
## Shell script for updating (rewriting) the full function index for the dispRity package
##########################
#SYNTAX: sh update.function.index.sh
##########################
#guillert(at)tcd.ie - 2021/08/09
##########################

## Creating the new function index
echo "file:line:function" > function.index.csv

for file in R/*.R
do
    ## Find replace all function declarations
    grep -nH "<- function" ${file} | sed 's/<- function.*//g' | sed 's/[[:space:]]//g' | sed 's/R\///g' | sed '/#/d' >> function.index.csv
    grep -nH "<-function" ${file} | sed 's/<-function.*//g' | sed 's/[[:space:]]//g' | sed 's/R\///g' >> function.index.csv
done

## Replace the : by commas to create the csv
sed 's/:/,/g' function.index.csv > tmp
mv tmp function.index.csv
