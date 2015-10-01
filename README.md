# dispRity
[![Build Status](https://travis-ci.org/TGuillerme/dispRity.svg?branch=release)](https://travis-ci.org/TGuillerme/dispRity)

A package for measuring disparity in `R`

**dispRity** is a `R` package allows to summarise ordinated matrices (e.g. MDS, PCA, PCO, PCoA) into single values.

## Installing dispRity
```r
#install.packages("devtools")
library(devtools)
install_github("TGuillerme/dispRity", ref="release")
library(dispRity)
```
The following installs the latest released version (see patch notes below). For the piping hot development version (not recommended), replace the `ref="release"` option by `ref="master"`.

<!-- ##### Patch notes <!-- A patch note is usless for now
* 2015/10/01 - v0.1.0
  * first release!
  
All patch notes can be seen [here](https://github.com/TGuillerme/dispRity/blob/master/patch_notes.md).
-->
## Using dispRity
Here is a really quick go through demo see the detailed vignette [here (in .Rnw)](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-tutorial.Rnw) or [here (in .pdf)](https://github.com/TGuillerme/dispRity/blob/master/dispRity-tutorial.0.1.0.pdf) for more details.

````r
## Loading the package and the demo data
library(dispRity) ; data(BeckLee_mat99) ; data(BeckLee_tree)

## Splitting the data
sliced_data <- time.series(BeckLee_mat99, BeckLee_tree, method = "continuous",
    model = "acctran", time = 5, FADLAD = MyData_ages)

## Bootstrapping the data
bootstrapped_data <- boot.matrix(sliced_data, 100)

## Calculating disparity
sum_of_ranges <- dispRity(bootstrapped_data, metric = c(sum, ranges))

## Summarising the results
summary(sum_of_ranges) ;plot(sum_of_ranges, type = "continuous")
````

#### Implemented metric
A list of implemented metrics (or soon to be implemented) is available [here](https://github.com/TGuillerme/dispRity/blob/master/metrics.md).


Authors
-------
[Thomas Guillerme](http://tguillerme.github.io)


<!--Citation
-------
A proper citation format will be availble soon
-->
