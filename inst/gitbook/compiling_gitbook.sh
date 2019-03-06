#!/bin/sh

rm dispRity_manual.Rmd

## Remove files before hand
rm -R _book/
rm -R _bookdown_files/

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

rm -R _bookdown_files/
