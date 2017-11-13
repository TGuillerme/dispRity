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
* 2017/11/13 - v0.4.1
  * Added a **Making stuff up!** chapter to the manual
  * *New* disparity metric: `ancestral.distance` to get the distance from taxa/nodes to their ancestors.
  * *New* function: `random.circle` for generating random circle coordinates (see example in `space.maker` for creating doughnut spaces!).
  * *New* function: `get.bin.ages` for getting the geological timescale of a tree (based on `geoscale`).
  * Added a `t0` argument to `time.subsamples` allowing to set the start age of the first subsample.
  * Allowing subsamples to contain less than three elements (up to 0!).
  * Fixed fuzzy match issues in `slice.tree`.
   
Previous patch notes and notes for the *next version* can be seen [here](https://github.com/TGuillerme/dispRity/blob/master/patch_notes.md).

Authors
-------

* [Thomas Guillerme](http://tguillerme.github.io)
* [Natalie Cooper](http://nhcooper123.github.io)
* [Mark Puttick](https://puttickbiology.wordpress.com/)

Citation
-------
If you are using this package, please cite the following Zenodo DOI (an associated paper will be added soon):

* Guillerme, T. (**2016**). dispRity: a package for measuring disparity in R. Zenodo. 10.5281/zenodo.55646

 [BibTeX](https://zenodo.org/record/55646/export/hx), [EndNote](https://zenodo.org/record/55646/export/xe), [DataCite](https://zenodo.org/record/55646/export/dcite3), [RefWorks](https://zenodo.org/record/55646/export/xw)

Acknowledgments
-------
Some ideas/functionalities/implementations in this package where implemented following the suggestions of [Natalie Cooper](http://nhcooper123.github.io/), [Graeme Lloyd](http://www.graemetlloyd.com/), [Dave Bapst](https://github.com/dwbapst/), [Andrew Jackson](http://www.tcd.ie/Zoology/research/research/theoretical/andrewjackson.php) and [Martin Brazeau](http://www.imperial.ac.uk/people/m.brazeau).
