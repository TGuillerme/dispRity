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

#### Vignettes
*  The package manual [here (in .Rnw)](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-manual.Rnw) or [here (in .pdf)](https://github.com/TGuillerme/dispRity/blob/master/dispRity-manual.pdf).
*  A tutorial for palaeobiologists [here (in .Rnw)](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-palaeo-demo.Rnw) or [here (in .pdf)](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-palaeo-demo.pdf).
*  A tutorial ecologists [here (in .Rnw)](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-ecology-demo.Rnw) or [here (in .pdf)](https://github.com/TGuillerme/dispRity/blob/master/dispRity-ecology-demo.pdf).
*  The disparity metrics [here (in .Rnw)](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-metrics.Rnw) or [here (in .pdf)](https://github.com/TGuillerme/dispRity/blob/master/dispRity-metrics.pdf).

#### Implemented metric
A list of implemented metrics (or soon to be implemented) is available [here](https://github.com/TGuillerme/dispRity/blob/master/metrics.md).

Authors
-------
[Thomas Guillerme](http://tguillerme.github.io)


Citation
-------
If you are using this package, please cite the following Zenodo DOI (an associated paper will be added soon):

* Guillerme, T. (**2015**). dispRity: a package for measuring disparity in R. Zenodo. 10.5281/zenodo.31742

[BibTeX](https://zenodo.org/record/31742/export/hx), [EndNote](https://zenodo.org/record/31742/export/xe), [DataCite](https://zenodo.org/record/31742/export/dcite3), [RefWorks](https://zenodo.org/record/31742/export/xw)
