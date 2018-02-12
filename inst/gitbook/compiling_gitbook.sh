#!/bin/sh

## Compile the html
R -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")' # HTML version

## Comment out the badges
sed 's/<!-- badges out start -->/<!-- badges out start/g' index.Rmd > index.tmp
sed 's/<!-- badges out end   -->/badges out end   -->/g' index.tmp > index.Rmd
rm index.tmp

## Compile the pdf
R -e 'bookdown::render_book("index.Rmd", "bookdown::pdf_book")' # PDF version

## Reactivate the badges
sed 's/<!-- badges out start/<!-- badges out start -->/g' index.Rmd > index.tmp
sed 's/badges out end   -->/<!-- badges out end   -->/g' index.tmp > index.Rmd
rm index.tmp


echo "#################################"
echo "#"
echo "# SOME FIGURES MIGHT NEED UPDATE"
echo "#"
echo "#################################"