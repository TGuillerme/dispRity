[![Build Status](https://travis-ci.org/TGuillerme/dispRity.svg?branch=release)](https://travis-ci.org/TGuillerme/dispRity)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.55646.svg)](https://doi.org/10.5281/zenodo.55646)

**dispRity** is a `R` package for summarising ordinated matrices (e.g. MDS, PCA, PCO or PCoA analysis).

<a href="https://figshare.com/articles/New_approaches_to_disparity-through-time_analysis/3437546"><img src="http://tguillerme.github.io/images/logo-FS.png" height="15" widht="15"/></a> 
Check out the [presentation](https://figshare.com/articles/New_approaches_to_disparity-through-time_analysis/3437546) or the [video](https://www.youtube.com/watch?v=ZzipKw8W8KQ) of some of the package's novelties.

## Installing dispRity
```r
if(!require(devtools)) install.packages("devtools")
library(devtools)
install_github("TGuillerme/dispRity", ref = "release")
library(dispRity)
```

The following installs the latest released version (see patch notes below). For the piping hot development version (not recommended), replace the `ref="release"` option by `ref="master"`. If you're using the `master` branch, see the latest developement in the [patch note](https://github.com/TGuillerme/dispRity/blob/master/patch_notes.md).

> A brand new version, v0.3 (*dispRity lite*), is in preparation and should be released early 2017. This version **highly** improves calculation speed and memory footprints of `dispRity` objects! Stay tuned! Check the [dev-testing branch](https://github.com/TGuillerme/dispRity/tree/dev-testing) for more info.

#### Vignettes
Note that some of the vignettes do not contain all the details of the improvements from version `0.2`. Refer to each function's manual for more information.
*  The package manual [here](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-manual.Rmd) [html](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-manual.html).
*  A tutorial for palaeobiologists [here](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-palaeo-demo.Rmd).
*  A tutorial ecologists [here](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-ecology-demo.Rmd).
*  The disparity metrics [here](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-metrics.Rmd).
*  Simulating morphological characters [here](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-simulate_data.Rmd).

Additionally, you can learn more about the structure of `dispRity` objects [here](https://github.com/TGuillerme/dispRity/blob/master/disparity_object.md).

##### Patch notes
* 2016/11/07 - v0.2.1 - *Simulating morphological matrices*
  * *new* utility function: `merge.time.series` for cleaning or merging time series
  * *new* vignette: `dispRity-simulate_data` on how to simulate morphological data in `dispRity`
  * *new* function: `check.morpho` for checking the how "realistic" the morphological matrices are
  * *new* function: `sim.morpho` generates morphological matrices 
  * *new* utility functions: `get.contrast.matrix` and `apply.inapplicable` functions for morphological matrices
  * minor graphical functions updates
  
Previous patch notes and the *next version* ones can be seen [here](https://github.com/TGuillerme/dispRity/blob/master/patch_notes.md).

#### Implemented metric
A list of implemented metrics is available [here](https://github.com/TGuillerme/dispRity/blob/master/doc/dispRity-metrics.Rmd). Note that the whole idea of `dispRity`'s architecture is to allow users to implement their own metrics though!

Authors
-------
[Thomas Guillerme](http://tguillerme.github.io)

Citation
-------
If you are using this package, please cite the following Zenodo DOI (an associated paper will be added soon):

* Guillerme, T. (**2016**). dispRity: a package for measuring disparity in R. Zenodo. 10.5281/zenodo.55646

 [BibTeX](https://zenodo.org/record/55646/export/hx), [EndNote](https://zenodo.org/record/55646/export/xe), [DataCite](https://zenodo.org/record/55646/export/dcite3), [RefWorks](https://zenodo.org/record/55646/export/xw)

Acknowledgments
-------
Some ideas/functionalities/implementations in this package where implemented following the suggestions of [Natalie Cooper](http://nhcooper123.github.io/), [Graeme Lloyd](http://www.graemetlloyd.com/), [Dave Bapst](http://webpages.sdsmt.edu/~dbapst/), [Andrew Jackson](http://www.tcd.ie/Zoology/research/research/theoretical/andrewjackson.php) and [Martin Brazeau](http://www.imperial.ac.uk/people/m.brazeau).
