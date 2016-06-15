[![Build Status](https://travis-ci.org/TGuillerme/dispRity.svg?branch=release)](https://travis-ci.org/TGuillerme/dispRity)
[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.31742.svg)](http://dx.doi.org/10.5281/zenodo.31742)

**dispRity** is a `R` package for summarising ordinated matrices (e.g. MDS, PCA, PCO or PCoA analysis).

## Installing dispRity
```r
if(!require(devtools)) install.packages("devtools")
library(devtools)
install_github("TGuillerme/dispRity", ref = "release")
library(dispRity)
```

The following installs the latest released version (see patch notes below). For the piping hot development version (not recommended), replace the `ref="release"` option by `ref="master"`. If you're using the `master` branch, see the latest developement in the [patch note](https://github.com/TGuillerme/dispRity/blob/master/patch_notes.md).

#### Vignettes
Note that some of the vignettes do not contain all the details of the improvements from version `0.2`. Refer to each function's manual for more information.
*  The package manual [here](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-manual.Rmd).
*  A tutorial for palaeobiologists [here](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-palaeo-demo.Rmd).
*  A tutorial ecologists [here](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-ecology-demo.Rmd).
*  The disparity metrics [here](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-metrics.Rmd).

Additionally, you can learn more about the structure of `dispRity` objects [here](https://github.com/TGuillerme/dispRity/blob/master/disparity_object.md).

##### Patch notes
* 2016/06/15 - v0.2.0
  * *new* utility functions: `pair.plot`, `scale.dispRity` and `sort.dispRity`.
  * *new* function: `space.maker` for creating some multidimensional spaces!
  * *new* disparity metrics: `convhull.surface`, `convhull.volume` and `hyper.volume`
  * *new* disparity test `null.test`.
  * *new* `plot.dispRity` arguments: `density` for controlling the polygons density and `add` for adding plots.
  * **removed** `type_discrete` argument in `plot.dispRity` and `type` argument can now be:
  	* `continuous` disparity curves.
  	* `box` for real boxplots.
  	* `lines` for the distribution verticale lines.
  	* `polygon` for the distribution boxes.
  * minor functions corrections for specific optional arguments combinations.
  * many updates to the functions manual and vignettes.
  * some algorithm are now improved for speed
  * disparity can now be calculated as a distribution (i.e. `dispRity`, `test.dispRity`, `plot.dispRity`, and `summary.dispRity` can now intake one or more distribution rather than just one or more single values of disparity; whether the data is bootstrapped or not).
  * `dispRity` can now intake `dispaRity` objects with level 2 disparity metrics.
  * `boot.matrix` and `dispRity` can now run in parallel.
  * `centroids` disparity metric can now intake a `centroid` argument for fixing the centroid point value.
  * `variances` and `ranges` disparity metrics can now intake a `k.root` argument for scaling the results.
  
Previous patch notes and the *next version* ones can be seen [here](https://github.com/TGuillerme/dispRity/blob/master/patch_notes.md).


#### Implemented metric
A list of implemented metrics is available [here](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-metrics.Rmd).

Authors
-------
[Thomas Guillerme](http://tguillerme.github.io)


Citation
-------
If you are using this package, please cite the following Zenodo DOI (an associated paper will be added soon):

* Guillerme, T. (**2015**). dispRity: a package for measuring disparity in R. Zenodo. 10.5281/zenodo.31742

 [BibTeX](https://zenodo.org/record/31742/export/hx), [EndNote](https://zenodo.org/record/31742/export/xe), [DataCite](https://zenodo.org/record/31742/export/dcite3), [RefWorks](https://zenodo.org/record/31742/export/xw)
