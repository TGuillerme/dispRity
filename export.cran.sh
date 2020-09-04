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
#cp README.md ${TMPPATH}/

## Copy the relevant folders
mkdir ${TMPPATH}/data/
cp data/* ${TMPPATH}/data/
mkdir ${TMPPATH}/man/
cp man/* ${TMPPATH}/man/
mkdir ${TMPPATH}/R/
cp R/* ${TMPPATH}/R/
mkdir ${TMPPATH}/src/
cp src/* ${TMPPATH}/src/

## Don't export tests to CRAN
# mkdir ${TMPPATH}/tests/
# cp tests/testthat.R ${TMPPATH}/tests/
# mkdir ${TMPPATH}/tests/testthat/
# cp tests/testthat/*.R* ${TMPPATH}/tests/testthat/

## Copy the vignettes (but not the gitbook!)
mkdir ${TMPPATH}/inst/
cp inst/CITATION ${TMPPATH}/inst/
cp inst/*.bib ${TMPPATH}/inst/
mkdir ${TMPPATH}/inst/vignettes
cp inst/vignettes/*.Rmd ${TMPPATH}/inst/vignettes/

## Remove the compiled source
rm src/*.o
rm src/*.so
rm src/*.rds

## Version number
version_number=$(grep "Version:" DESCRIPTION | sed 's/Version: //g')

## CHANGE THE WARNING zzz.R
sed 's/# //g' R/zzz.R | sed 's/GitHub release./This is the CRAN release version ('"$version_number"') of the package.\\nFor more functionalities, news, vignettes and releases,\\nvisit https:\/\/github.com\/TGuillerme\/dispRity/g' > ${TMPPATH}/R/zzz.R

## Add ssptest.support and remove dependencies 
cp ~/Packaging/CRAN/Support/ssptest.support.R ${TMPPATH}/R/

## Remove spptest from model.test.fun.R
sed 's/spptest::create_curve_set/create_curve_set/g' ${TMPPATH}/R/model.test_fun.R | sed -e 's/spptest::rank_envelope/rank_envelope/g' > export.cran.tmp
mv export.cran.tmp ${TMPPATH}/R/model.test_fun.R

## Remove remotes from DESCRIPTION
line_remove=$(grep -n "Remotes" ${TMPPATH}/DESCRIPTION | sed -e 's/:Remotes://g')
sed ''"${line_remove}"'d' ${TMPPATH}/DESCRIPTION > export.cran.tmp
line_remove=$(grep -n "github::myllym/spptest@no_fastdepth" export.cran.tmp | sed -e 's/:    github::myllym\/spptest@no_fastdepth//g')
sed ''"${line_remove}"'d' export.cran.tmp > export.cran.tmp2
## Remove imports
line_remove=$(grep -n "spptest" export.cran.tmp2 | sed -e 's/:[[:space:]]spptest//g')
sed ''"${line_remove}"'d' export.cran.tmp2 > export.cran.tmp
## Remove coma to last import
let "line_remove -= 1"
sed ''"${line_remove}"'s/,//' export.cran.tmp > ${TMPPATH}/DESCRIPTION

## Remove spptest from NAMESPACE
line_remove=$(grep -n "importFrom(\"spptest\"," ${TMPPATH}/NAMESPACE | sed -e 's/:importFrom("spptest", "create_curve_set", "rank_envelope")//g')
sed ''"${line_remove}"'d' ${TMPPATH}/NAMESPACE > export.cran.tmp
## Add "is" to methods in NAMESPACE
sed 's/importFrom("methods", "hasArg"/importFrom("methods", "hasArg", "is"/g' export.cran.tmp > ${TMPPATH}/NAMESPACE
rm export.cran.tmp
rm export.cran.tmp2

## Compile the package
cd cran_tmp/
R CMD build --resave-data dispRity

## Check the package
R CMD check --as-cran dispRity_*.tar.gz
#--install-args=_R_CHECK_LENGTH_1_LOGIC2_ 
#--as-cran 

## Check compile
if grep -e 'ERROR' -e 'WARNING' dispRity.Rcheck/00check.log 
then
    echo "You broke it again! Bordel!"
    cd ..
else

    if grep -e 'NOTE'  dispRity.Rcheck/00check.log 
    then
        echo "Check out the NOTE(s) before submitting!"
    else
        echo "Nice one: it compiles smoothly!"
    fi    
    echo "The tar ball has been moved to ~/Packaging/CRAN/"
    mv dispRity_*.tar.gz ~/Packaging/CRAN/
    cd ..
    rm -R cran_tmp/
fi

