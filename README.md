[![Build Status](https://travis-ci.org/TGuillerme/dispRity.svg?branch=release)](https://travis-ci.org/TGuillerme/dispRity)
[![codecov](https://codecov.io/gh/TGuillerme/dispRity/branch/master/graph/badge.svg)](https://codecov.io/gh/TGuillerme/dispRity)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.846254.svg)](https://doi.org/10.5281/zenodo.846254)

**dispRity** is a `R` modular package for measuring disparity from multidimensional matrices.

<a href="https://figshare.com/articles/New_approaches_to_disparity-through-time_analysis/3437546"><img src="http://tguillerme.github.io/images/logo-FS.png" height="15" widht="15"/></a> 
Check out the [presentation](https://figshare.com/articles/New_approaches_to_disparity-through-time_analysis/3437546) or the [video](https://www.youtube.com/watch?v=ZzipKw8W8KQ) of some of the package's novel features.

## Installing dispRity
```r
if(!require(devtools)) install.packages("devtools")
library(devtools)
install_github("TGuillerme/dispRity", ref = "release")
library(dispRity)
```

The following installs the latest release of dispRity (see patch notes below). For the piping hot development version (not recommended), replace the `ref = "release"` option with `ref = "master"`.
If you're using the `master` branch, see the [patch notes](https://github.com/TGuillerme/dispRity/blob/master/patch_notes.md) for the latest developments.

> Installation problems? Make sure you're using the latest R version (or at least `> 3.3.0`) and try again.

> To optimise your `dispRity` usage and speed, make sure you have the last version of `ape` installed (`5.0` - [download here](https://cran.r-project.org/web/packages/ape/index.html))!
If you get a corruption error while updateding your packages, remove both (`remove.packages(c("ape", "dispRity"))`), quit and relaunch `R` and simply reinstall `dispRity` (`ape` should update automatically).

#### Vignettes

See the package manual online [here](https://rawgit.com/TGuillerme/dispRity/master/inst/gitbook/_book/index.html) or download the pdf version [here](https://github.com/TGuillerme/dispRity/raw/master/inst/gitbook/_book/dispRity_manual.pdf).

Additionally, you can learn more about the structure of `dispRity` objects [here](https://github.com/TGuillerme/dispRity/blob/master/disparity_object.md).

##### Patch notes
* 2017/12/20 - v0.5.0 *covered with tests*
  * `custom.subset` can now automatically create clade groups if a `phylo` object is passed to `group`.
  * Changed calls to `stats::dist` to `vegan::vegdist` to allow more distances to be passed through `methods` arguments.
  * *New* utility function: `extinction.subsets`, to get the list to be passed to `test.dispRity` for testing the effect of extinction.
  * *New* test function: `dtt.dispRity`, a wrapper for [`geiger::dtt`](https://github.com/mwpennell/geiger-v2). This version is slower that `geiger::dtt` but allows any univariate disparity metric!
  * *New* test function: `adonis.dispRity`, a wrapper for [`vegan::adonis`](https://github.com/vegandevs/vegan).
  * `slice.tree` can now slice through a single edge.
  * Various small speed improvements.
  * Correct behaviour in `tree.age` to estimate correct ages for trees with fossils only.
  * *New* utility function: `crown.stem` for separating a tree into crown and stem groups.
  * *New* disparity metric: `span.tree.length` the length of the minimum spanning tree.
  * *New* disparity metric: `pairwise.dist`: the element's pairwise distances.
  * *New* disparity metric: `radius`: the radius of each dimensions.
  * *New* disparity metric: `n.ball.volume`: the *n*-dimensional sphere or ellipsoid volume.
  * **Change name** throughout the package, `subsample` is now replaced by `subset` (e.g. `time.subsamples` is now renamed `time.subsets`, `data$subsamples` is now `data$subsets`, etc...)
  * **Changed argument** in `time.subsets`, `model = "gradual"` is now replaced by `model = "proximity"` and `model = "punctuated"` is now replaced by `model = "random"`.
  * **New argument** in `time.subsets`, `model = "punctuated"` and `model = "gradual"` that retain the probability of being either the descendant or the ancestor. This probability is passed to `boot.matrix` .
   
Previous patch notes and notes for the *next version* can be seen [here](https://github.com/TGuillerme/dispRity/blob/master/patch_notes.md).

Authors
-------

* [Thomas Guillerme](http://tguillerme.github.io)
* [Natalie Cooper](http://nhcooper123.github.io)
* [Mark Puttick](https://puttickbiology.wordpress.com/)

Citation
-------
If you are using this package, please cite the following Zenodo DOI (an associated paper will be added soon):

* Guillerme, T. (**2015**). dispRity: a package for measuring disparity in R. Zenodo. 10.5281/zenodo.31742

 [BibTeX](https://zenodo.org/record/31742/export/hx), [EndNote](https://zenodo.org/record/31742/export/xe), [DataCite](https://zenodo.org/record/5563174246/export/dcite3), [RefWorks](https://zenodo.org/record/31742/export/xw)

Acknowledgments
-------
Some ideas/functionalities/implementations in this package where implemented following the suggestions of [Natalie Cooper](http://nhcooper123.github.io/), [Graeme Lloyd](http://www.graemetlloyd.com/), [Dave Bapst](https://github.com/dwbapst/), [Andrew Jackson](http://www.tcd.ie/Zoology/research/research/theoretical/andrewjackson.php) and [Martin Brazeau](http://www.imperial.ac.uk/people/m.brazeau).
