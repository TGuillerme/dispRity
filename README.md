# dispRity
[![Build Status](https://travis-ci.org/TGuillerme/dispRity.svg?branch=release)](https://travis-ci.org/TGuillerme/dispRity)
[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.31742.svg)](http://dx.doi.org/10.5281/zenodo.31742)

**dispRity** is a `R` package for summarising ordinated matrices (e.g. MDS, PCA, PCO or PCoA analysis).

## Installing dispRity
```r
#install.packages("devtools")
library(devtools)
install_github("TGuillerme/dispRity", ref = "release")
library(dispRity)
```
The following installs the latest released version (see patch notes below). For the piping hot development version (not recommended), replace the `ref="release"` option by `ref="master"`. If you're using the `master` branch, see the latest developement in the [patch note](https://github.com/TGuillerme/dispRity/blob/master/patch_notes.md).

##### Patch notes
* 2015/10/08 - v0.1.1
  * Fixed many error/warning messages
  * `plot.dispRity` options improved (rarefaction + default)
  * `cust.series` can now intake multiple factors columns
  * Added exampple for ecological data
  * Changed `taxa` to `elements`
  * `boot.matrix`, `dispRity`, `summary` and `plot` now also include observed values
  * `plot` has now a `observed` option to plot the observed disparity
  * `plot` option `diversity` has been renamed `elements`
  
Previous patch notes and the *next version* ones can be seen [here](https://github.com/TGuillerme/dispRity/blob/master/patch_notes.md).

## Using dispRity
Here is a really quick go through demo see the detailed vignette below.

````r
## Loading the package and the demo data
library(dispRity) ; data(BeckLee_mat99) ; data(BeckLee_tree) ; data(BeckLee_ages)

## Splitting the data
sliced_data <- time.series(BeckLee_mat99, BeckLee_tree, method = "continuous",
    model = "acctran", time = c(130,100,60,30,0), FADLAD = BeckLee_ages)

## Bootstrapping the data
bootstrapped_data <- boot.matrix(sliced_data, 100)

## Calculating disparity
sum_of_ranges <- dispRity(bootstrapped_data, metric = c(sum, ranges))

## Summarising the results
summary(sum_of_ranges) ; plot(sum_of_ranges)

## Looking at the differences between series
test.dispRity(sum_of_ranges, wilcox.test, "sequential")
````

#### Implemented metric
A list of implemented metrics (or soon to be implemented) is available [here](https://github.com/TGuillerme/dispRity/blob/master/metrics.md).

#### Vignettes
*  A general tutorial [here (in .Rnw)](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-tutorial.Rnw) or [here (in .pdf)](https://github.com/TGuillerme/dispRity/blob/master/dispRity-tutorial.pdf).
*  An tutorial for palaeobiologists [here (in .Rnw)](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-palaeo-demo.Rnw) or [here (in .pdf)](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-palaeo-demo.pdf).
*  An ecological tutorial [here (in .Rnw)](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-ecology-demo.Rnw) or [here (in .pdf)](https://github.com/TGuillerme/dispRity/blob/master/dispRity-ecology-demo.pdf).
*  The disparity metrics [here (in .Rnw)](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-metrics.Rnw) or [here (in .pdf)](https://github.com/TGuillerme/dispRity/blob/master/dispRity-metrics.pdf).


Authors
-------
[Thomas Guillerme](http://tguillerme.github.io)


Citation
-------
If you are using this package, please cite the following Zenodo DOI (an associated paper will be added soon):

* Guillerme, T. (**2015**). dispRity: a package for measuring disparity in R. Zenodo. 10.5281/zenodo.31742

[BibTeX](https://zenodo.org/record/31742/export/hx), [EndNote](https://zenodo.org/record/31742/export/xe), [DataCite](https://zenodo.org/record/31742/export/dcite3), [RefWorks](https://zenodo.org/record/31742/export/xw)
