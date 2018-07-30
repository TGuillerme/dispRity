##########################
# Script for exporting the package in CRAN format
##########################
#SYNTAX: sh export.cran.sh -e <exclude>
#with:
#-e <exclude> the command for running the script (e.g. sh, etc. - for more than one module, separate them with commas)
##########################
#guillert(at)tcd.ie - 2018/07/30
##########################

#INPUT
## Input values
while [[ $# -gt 1 ]]
do
key="$1"

case $key in
    -e|--exclude)
        EXCLUDE="$2"
        ;;
        *)

        ;;
esac
shift
done

## Create the temporary CRAN folder
mkdir cran_tmp
mkdir cran_tmp/dispRity
TMPPATH="cran_tmp/dispRity"

## Copy the relevant files
cp DESCRIPTION ${TMPPATH}/
cp NAMESPACE ${TMPPATH}/
cp LICENSE ${TMPPATH}/
cp NEWS.md ${TMPPATH}/
cp README.md ${TMPPATH}/

## Copy the relevant folders
mkdir ${TMPPATH}/data/
cp data/* ${TMPPATH}/data/
mkdir ${TMPPATH}/man/
cp man/* ${TMPPATH}/man/
mkdir ${TMPPATH}/R/
cp R/* ${TMPPATH}/R/
mkdir ${TMPPATH}/src/
cp src/* ${TMPPATH}/src/
mkdir ${TMPPATH}/tests/
cp tests/testthat.R ${TMPPATH}/tests/
mkdir ${TMPPATH}/tests/testthat/
cp tests/testthat/*.R* ${TMPPATH}/tests/testthat/

## Copy the vignettes (but not the gitbook!)
mkdir ${TMPPATH}/inst/
cp inst/CITATION ${TMPPATH}/inst/
cp inst/*.bib ${TMPPATH}/inst/
mkdir ${TMPPATH}/inst/vignettes
cp inst/vignettes/* ${TMPPATH}/inst/vignettes/

## Remove the compiled source
rm src/*.o
rm src/*.so
rm src/*.rds


##
# CHANGE THE WARNING zzz.R

##
# CHANGE THE VIGNETTES

##
# Exclude some code

# ## Exclude some code!
# if [ -z ${MODULES+x} ]
# then
#     silent="1"
# else 
#     echo "## Load modules" >> ${SCRIPT}.job
#     if echo $MODULES | grep , > /dev/null
#     then
#         echo "module load $(echo $MODULES | sed 's/,/ /g')" >> ${SCRIPT}.job
#         echo "" >> ${SCRIPT}.job
#     else
#         echo "module load $MODULES" >> ${SCRIPT}.job
#         echo "" >> ${SCRIPT}.job
#     fi
# fi


## Compile the package
cd cran_tmp/
R CMD build dispRity

## Check the package
R CMD check dispRity_*.tar.gz
