Release:

[![Build Status](https://travis-ci.org/TGuillerme/dispRity.svg?branch=release)](https://travis-ci.org/TGuillerme/dispRity)
[![codecov](https://codecov.io/gh/TGuillerme/dispRity/branch/release/graph/badge.svg)](https://codecov.io/gh/TGuillerme/dispRity)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![develVersion](https://img.shields.io/badge/devel%20version-1.5.3-green.svg?style=flat)](https://github.com/TGuillerme/dispRity/tree/release)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1186467.svg)](https://doi.org/10.5281/zenodo.1186467)


Development (master):

[![Build Status](https://travis-ci.org/TGuillerme/dispRity.svg?branch=master)](https://travis-ci.org/TGuillerme/dispRity)
[![codecov](https://codecov.io/gh/TGuillerme/dispRity/branch/master/graph/badge.svg)](https://codecov.io/gh/TGuillerme/dispRity)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![develVersion](https://img.shields.io/badge/devel%20version-1.5.4-green.svg?style=flat)](https://github.com/TGuillerme/dispRity)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1186467.svg)](https://doi.org/10.5281/zenodo.1186467)

CRAN:

[![minimal R version](https://img.shields.io/badge/R%3E%3D-4.0.0-6666ff.svg)](https://cran.r-project.org/)
[![cran version](http://www.r-pkg.org/badges/version/dispRity)](https://cran.r-project.org/package=dispRity)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/dispRity)](https://github.com/metacran/cranlogs.app)
![](http://cranlogs.r-pkg.org/badges/dispRity)

### **`dispRity`** is a `R` modular package for measuring disparity from multidimensional matrices.

<a href="https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/2041-210X.13022"><img src="http://tguillerme.github.io/images/OA.png" height="15" widht="15"/></a> 
Check out the [paper](https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/2041-210X.13022) associated with this package.

<a href="https://figshare.com/articles/New_approaches_to_disparity-through-time_analysis/3437546"><img src="http://tguillerme.github.io/images/logo-FS.png" height="15" widht="15"/></a> 
Check out the [presentation](https://figshare.com/articles/New_approaches_to_disparity-through-time_analysis/3437546) or the [video](https://www.youtube.com/watch?v=ZzipKw8W8KQ) of some of the package's novel features.

<!-- Link available until August 2019
https://programme.europa-organisation.com/slides/programme_jointCongressEvolBiology-2018/webconf/683_19082018_1140_antigone1_Thomas_Guillerme_480/index.html
 -->

## Installing dispRity

```r
if(!require(devtools)) install.packages("devtools")
library(devtools)
install_github("TGuillerme/dispRity", ref = "release")
library(dispRity)
```

The following installs the latest release of dispRity (see patch notes below). For the piping hot development version (not recommended), replace the `ref = "release"` option with `ref = "master"`.
If you're using the `master` branch, see the [patch notes](https://github.com/TGuillerme/dispRity/blob/master/NEWS.md) for the latest developments.

The package is available in the [CRAN Task Views in Phylogenetics](https://CRAN.R-project.org/view=Phylogenetics).

> Can't install the latest version of `dispRity` from the CRAN? 

The latest `dispRity` package version 1.5 requires R 4.0 to run. If you don't have your R version updated to 4.0 you can either:
  * 1) update your R version to the latest; this is  suggested: since the amazing R CRAN team always update their software for a good reason (and for free!);
  * 2) if for some reason you really can't update your R version, you can simply download the released github version of the package using: `devtools::install_github("TGuillerme/dispRity", ref = "release")`.


## Vignettes and manuals

A detailed vignette is available [online](https://tguillerme.github.io/dispRity.html) or as a [pdf](https://github.com/TGuillerme/dispRity/blob/master/inst/gitbook/_book/dispRity_manual.pdf):

<a href="https://tguillerme.github.io/dispRity.html"><img src="http://tguillerme.github.io/images/rawgit.png" height="30"/></a> <a href="https://github.com/TGuillerme/dispRity/blob/master/inst/gitbook/_book/dispRity_manual.pdf"><img src="http://tguillerme.github.io/images/pdf.gif" height="30"/></a> 
 
Otherwise, each functions has a detailed associated manual with examples in `R` (`?which.function`).

Additionally, you can learn more about the structure of `dispRity` objects [here](https://github.com/TGuillerme/dispRity/blob/master/disparity_object.md).

You can download the workshop follow-along sheet [here](https://raw.githubusercontent.com/TGuillerme/dispRity/master/inst/vignettes/dispRity_workhop_code.Rmd)(use right click > save link as...) or visualise it in html [here](https://raw.githack.com/TGuillerme/dispRity/master/inst/vignettes/dispRity_workhop_code.html).

### Disparity/dissimilarity metrics/indices?

Not sure what disparity metric to use?
Not sure what a disparity metric is in the first place?
Check out this pre-print on selecting the best metric for your specific question on [biorXiv](https://www.biorxiv.org/content/10.1101/801571v1) or the [`moms` shiny app](https://tguillerme.shinyapps.io/moms/).
You can also find more information in the [`dispRity` manual](https://rawcdn.githack.com/TGuillerme/dispRity/c94452e6877fbb274eb0a4ff1363272a6297a9ee/inst/gitbook/_book/details-of-specific-functions.html#disparity-metrics).
<!-- biorXiv preprint -->

## Latest major patch notes
* 2020/09/25 v1.5 *between groups*

  * *New* function: `multi.ace` for performing fast ancestral character estimations on multiple matrices (based on `castor::asr_mk_model`).
  * *New* function: `reduce.space`, a function to modify trait spaces imported from the [`moms` shiny app](https://github.com/TGuillerme/moms). This function comes with a new reduction algorithm: the "evenness" algorithm for flattening the curve (thanks to Gavin Thomas for the suggestion).
  * *New* function: `test.metric` (and associated `plot`, `print` and `summary` functions), to apply the `reduce.space` function on a specific space and metric to test whether a metric is picking up specific changes in trait space.
  * the `dispRity` function can now use `"between.groups"` metrics to calculate disparity between groups rather than within groups. The `make.metric` function is now modified to allow detection of metrics that can be applied between groups.
  * *New* metric: `group.dist`, a dimension level 1 metric for between groups that measures the distance between two groups. By default, this is the minimum distance but the function takes the `probs` argument allowing the distance to be between, says, the 95% CI (`probs = c(0.025, 0.975))`) or between the centroids (`probs = c(0.5)`).
  * *New* metric: `point.dist`, a dimension level 2 metric for between groups that measures the distance between the rows in `matrix` to a point in `matrix2`. That point is the centroid by default but the `point` argument can take any function.
  * The `dispRity` package now depends on `R (>= 4.0.0)`.
  * Many updates to the `dispRity` manual.
  * Many minor speed improvements across the package
  * Simplified syntax for the internal `plot.dispRity` S3 methods (for a potential `ggpRity`?). These changes should not be apparent at the user level but see the two removed options below:
  * **removed** option in `plot.dispRity`: the `chrono.subsets` option (`TRUE`/`FALSE`) has now been removed. The time-slicing-ness is now automatically detected or can be specified by the user normally through `xlab`.
  * **removed** option in `plot.dispRity`: the `ylim`, `ylab`, `xlab` and `col` options have now been removed. They are now handled through `...` as normal generic `plot(...)` arguments.
  * **removed** option in `plot.dispRity`: the `elements.pch` option has now been removed. The `pch` of the plotted elements can now be passed like other options directly to elements (e.g. `elements = list(pch = 15)`).
  * **removed** option in `plot.dispRity`: the `dimensions`, `matrix`, `nclass` and `coeff` options have now been removed. Any options for dual class plots (`randtest`, `dtt`, `model.test`, `type = "preview"` etc...) are now handled through the generic `specific.args` argument.
  * *New* option in `dtt.dispRity`: `scale.time` allowing to scale the time axis (like in `geiger::dtt`) or not.
  * when plotting `chrono.subsets` `dispRity` objects, the x label ticks are now rounded if possible (for nicer looking plots!).
  * when using automatic `chrono.subsets` time slices, the name of the time slices (their age) is now rounded for aesthetics.

Previous patch notes and notes for the *next version* can be seen [here](https://github.com/TGuillerme/dispRity/blob/master/NEWS.md).

Authors and contributors
-------

* [Thomas Guillerme](http://tguillerme.github.io)
* [Natalie Cooper](http://nhcooper123.github.io)
* [Mark Puttick](https://puttickbiology.wordpress.com/)

Citations
-------
If you are using this package, please cite the paper:

* Guillerme, T. (**2018**) dispRity: a modular R package for measuring disparity. *Methods in Ecology and Evolution*. [doi:10.1111/2041-210X.13022](https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/2041-210X.13022)
    ##### [Export citation](https://besjournals.onlinelibrary.wiley.com/action/showCitFormats?doi=10.1111%2F2041-210X.13022)

Also don't forget to cite `R` and consider citing the `ape` package since `dispRity` heavily relies on it (and, generally, cite all the `R` packages you use!):

* Paradis E. & Schliep K. (**2019**). ape 5.0: an environment for modern phylogenetics and evolutionary analyses in R. Bioinformatics 35: 526-528.
* R Core Team (**2020**). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
    ##### or use `citation()` and `citation("ape")` in `R` to get the latest citation format

To cite the [`dispRity` manual](https://rawgit.com/TGuillerme/dispRity/master/inst/gitbook/_book/dispRity_manual.pdf), please use:

* Guillerme, T. & Cooper, N. (**2018**) dispRity manual. *figshare*. Preprint. 10.6084/m9.figshare.6187337.v1
    ##### [BibTex](https://figshare.com/articles/6187337/1/citations/bibtex), [EndNote](https://figshare.com/articles/6187337/1/citations/endnote), [RefWorks](https://figshare.com/articles/6187337/1/citations/refworks), [DataCite](https://figshare.com/articles/6187337/1/citations/datacite), [more...](https://figshare.com/articles/dispRity_manual/6187337)

To cite the [time slicing method](https://onlinelibrary.wiley.com/doi/abs/10.1111/pala.12364) from the `chrono.subsets` function, please use:

* Guillerme, T. & Cooper, N. (**2018**) Time for a rethink: time sub‐sampling methods in disparity‐through‐time analyses. *Palaeontology*, 61: 481-493. [doi:10.1111/pala.12364](https://onlinelibrary.wiley.com/doi/abs/10.1111/pala.12364)
    ##### [Export citation](https://onlinelibrary.wiley.com/action/showCitFormats?doi=10.1111%2Fpala.12364)

Acknowledgments
-------
Some ideas/functionalities/implementations in this package where implemented following the suggestions of [Natalie Cooper](http://nhcooper123.github.io/), [Graeme Lloyd](http://www.graemetlloyd.com/), [Dave Bapst](https://github.com/dwbapst/), [Andrew Jackson](https://www.tcd.ie/Zoology/people/jacksoan) and [Martin Brazeau](http://www.imperial.ac.uk/people/m.brazeau).


Examples of papers using the `dispRity` package
-------

The `dispRity` package was cited in [these papers](https://scholar.google.co.uk/scholar?cites=13311379491028410826&as_sdt=2005&sciodt=0,5&hl=en). Below is a list of specific papers using specific functionalities. These papers do not _only_ use the functionalities highlighted here (and the author do much more awesome science!) but this is just a list of references if you want an independent guide on how to use these functions:

#### Measuring disparity with `dispRity` (using multiple metrics!)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=The+stem-archosaur+evolutionary+radiation+in+South+America&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Ezcurra MD, Montefeltro FC, Pinheiro FL, Trotteyn MJ, Gentil AR, Lehmann OE, Pradelli LA. The stem-archosaur evolutionary radiation in South America. Journal of South American Earth Sciences. 2020 Oct 7:102935. *Journal of South American Earth Sciences*. **2020** [DOI: 10.1016/j.jsames.2020.102935](https://www.sciencedirect.com/science/article/pii/S0895981120304788#bib109)

#### Using different time binning with `chrono.subsets`

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Crocodylomorph+cranial+shape+evolution+and+its+relationship+with+body+size+and+ecology&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Godoy PL. Crocodylomorph cranial shape evolution and its relationship with body size and ecology. *Journal of Evolutionary Biology*. **2020** [DOI: 10.1111/jeb.13540](https://onlinelibrary.wiley.com/doi/full/10.1111/jeb.13540)

#### Simulating discrete morphological data with `sim.morpho`:

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Morphological+disparity+in+theropod+jaws%3A+comparing+discrete+characters+and+geometric+morphometrics&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Schaeffer J, Benton MJ, Rayfield EJ, Stubbs TL. Morphological disparity in theropod jaws: comparing discrete characters and geometric morphometrics. Palaeontology. **2020**. [DOI: 10.1111/pala.12455](https://onlinelibrary.wiley.com/doi/full/10.1111/pala.12455)

#### Calculating the Bhattacharrya Coefficient with `bhatt.coeff`:

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=A+Kuramoto+model+of+self-other+integration+across+interpersonal+synchronization+strategies&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Heggli OA, Cabral J, Konvalinka I, Vuust P, Kringelbach ML. A Kuramoto model of self-other integration across interpersonal synchronization strategies. *PLoS computational biology*. **2019** [DOI:10.1371/journal.pcbi.1007422](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1007422)

##### Comparisons between groups using `custom.subsets` and `test.dispRity`:

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Phylogenomics%2C+biogeography+and+morphometrics+reveal+rapid+phenotypic+evolution+in+pythons+after+crossing+Wallace%27s+line&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Esquerré D, Donnellan S, Brennan IG, Lemmon AR, Lemmon EM, Zaher H, Grazziotin FG, Keogh JS. Phylogenomics, biogeography and morphometrics reveal rapid phenotypic evolution in pythons after crossing Wallace’s line. *Systematic Biology*. **2019** [DOI: 10.1093/sysbio/syaa024](https://doi.org/10.1093/sysbio/syaa024)

#### Bootstrapping data and comparing groups in ecospace using `boot.matrix`:

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&sciodt=0%2C5&cites=13311379491028410826&scipsc=&q=Morphological+discontinuous+variation+and+disparity+in+Lutzomyia+%28Tricholateralis%29+cruciata+Coquillett%2C+1907+are+not+related+to+contrasting+environmental+%E2%80%A6&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
de Oca-Aguilar AC, De Luna E, Rebollar-Téllez EA, Piermarini PM, Ibáñez-Bernal S. Morphological discontinuous variation and disparity in Lutzomyia (Tricholateralis) cruciata Coquillett, 1907 are not related to contrasting environmental factors in two biogeographical provinces. *Zoomorphology*. **2019** [DOI:10.1007/s00435-019-00450-8](https://link.springer.com/article/10.1007/s00435-019-00450-8)

#### Simulating disparity through time using `dtt.dispRity`:

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=he+Ecological+and+Evolutionary+Drivers+of+Spatial+Biodiversity+Patterns&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Skeels A. The Ecological and Evolutionary Drivers of Spatial Biodiversity Patterns. Doctoral Thesis at the Australian National University **2020** [Open Researcg](https://openresearch-repository.anu.edu.au/bitstream/1885/204921/1/Alexander_Skeels_Thesis_Revisions.pdf)

#### Using the wrapper `disparity.per.group` function:

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Diet+variability+among+insular+populations+of+Podarcislizards+reveals+diverse+strategies+to+face+resource%E2%80%90limited+environments&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Taverne M, Fabre AC, King‐Gillies N, Krajnović M, Lisičić D, Martin L, Michal L, Petricioli D, Štambuk A, Tadić Z, Vigliotti C. Diet variability among insular populations of Podarcis lizards reveals diverse strategies to face resource‐limited environments. *Ecology and Evolution*. **2019** [DOI:10.1002/ece3.5626](https://onlinelibrary.wiley.com/doi/pdf/10.1002/ece3.5626).


