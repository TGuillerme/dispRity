Release:

[![Build Status](https://travis-ci.org/TGuillerme/dispRity.svg?branch=release)](https://travis-ci.org/TGuillerme/dispRity)
[![codecov](https://codecov.io/gh/TGuillerme/dispRity/branch/release/graph/badge.svg)](https://codecov.io/gh/TGuillerme/dispRity)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![develVersion](https://img.shields.io/badge/devel%20version-1.6.0-green.svg?style=flat)](https://github.com/TGuillerme/dispRity/tree/release)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1186467.svg)](https://doi.org/10.5281/zenodo.1186467)


Development (master):

[![Build Status](https://travis-ci.org/TGuillerme/dispRity.svg?branch=master)](https://travis-ci.org/TGuillerme/dispRity)
[![codecov](https://codecov.io/gh/TGuillerme/dispRity/branch/master/graph/badge.svg)](https://codecov.io/gh/TGuillerme/dispRity)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![develVersion](https://img.shields.io/badge/devel%20version-1.6.0-green.svg?style=flat)](https://github.com/TGuillerme/dispRity)
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

### Workshop

You can download the workshop follow-along sheet [here](https://raw.githubusercontent.com/TGuillerme/dispRity/master/inst/vignettes/dispRity_workhop_code.Rmd)(use right click > save link as...) or visualise it in html [here](https://raw.githack.com/TGuillerme/dispRity/master/inst/vignettes/dispRity_workhop_code.html).

### Disparity/dissimilarity metrics/indices?

Not sure what disparity metric to use?
Not sure what a disparity metric is in the first place?
Check out this pre-print on selecting the best metric for your specific question on [biorXiv](https://www.biorxiv.org/content/10.1101/801571v1) or the [`moms` shiny app](https://tguillerme.shinyapps.io/moms/).
You can also find more information in the [`dispRity` manual](https://rawcdn.githack.com/TGuillerme/dispRity/c94452e6877fbb274eb0a4ff1363272a6297a9ee/inst/gitbook/_book/details-of-specific-functions.html#disparity-metrics).
<!-- biorXiv preprint -->

## Latest major patch notes
* 2021/04/16 v1.6 *dispRitree*

  * *New* metric: `projections` that allows to measure elements' projection on an arbitrary axis (or their distance from this axis with `measure = "distance"`).
  * *New* metric: `projections.tree` that allows to measure elements' projection on axis between elements of a given tree.
  * *New* metric: `edge.length.tree` the edge length from each element given a tree (with the option `to.root = TRUE/FALSE` to measure the edge length from the element to the root of the tree (default = TRUE) or the nearest ancestor (FALSE).
  * You can now save the shifts results in `test.metric` with `save.steps` and then visualise them with `plot.dispRity` along side the disparity metric test results.
  * *New* utility function `n.subsets` to directly get the number of subsets in a `dispRity` object.
  * *New* statistical test: `randtest.dispRity` that is a wrapper for `ade4::randtest` applied to `dispRity` objects (not dissimilar from `null.test`).
  * Six more demo datasets have been added to the package! These datasets are the ones used in [Guillerme et al. 2020](https://onlinelibrary.wiley.com/doi/full/10.1002/ece3.6452) and published originally in [Beck & Lee 2014](https://royalsocietypublishing.org/doi/full/10.1098/rspb.2014.1278) (that one was originally the only demo dataset in the package), [Wright 2017](https://www.cambridge.org/core/journals/journal-of-paleontology/article/bayesian-estimation-of-fossil-phylogenies-and-the-evolution-of-early-to-middle-paleozoic-crinoids-echinodermata/E37972902541CD0995AAD08A1122BD54), [Marcy et al. 2016](https://link.springer.com/article/10.1186/s12862-016-0782-1), [Hopkins & Pearson 2016](https://www.researchgate.net/profile/Melanie_Hopkins3/publication/320543447_Non-linear_ontogenetic_shape_change_in_Cryptolithus_tesselatus_Trilobita_using_three-dimensional_geometric_morphometrics/links/59f7307d0f7e9b553ebd5e03/Non-linear-ontogenetic-shape-change-in-Cryptolithus-tesselatus-Trilobita-using-three-dimensional-geometric-morphometrics.pdf), [Jones et al. 2015](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2745.12405), [Healy et al. 2019](https://www.nature.com/articles/s41559-019-0938-7). Thanks to all these authors for their open science work!
  * `dispRity` objects now have a reserved `$tree` component that contain any number of trees attached to the data. This allows any function to use the reserved argument name `tree` to extract directly the relevant tree from the `dispRity` object, for functions like `chrono.subsets` or metrics like `ancestral.dist`! To help manipulate the `tree` component of the `dispRity` object, you can now use the new utility functions `add.tree`, `get.tree` and `remove.tree`.
  * Reverted R version requirement from `4.0` back to `3.6` following [Joseph Brown's issue](https://github.com/TGuillerme/dispRity/issues/107) and [fix](https://github.com/TGuillerme/dispRity/pull/108).
  * `reduce.space` `"random"` algorithm now outputs a named logical vector (like the other algorithms!).
  * remove the `"only"` text when printing `dispRity` objects that contains "only" matrices (even though that can be 10k matrices!).
  * added a dedicated behaviour to `summary.dispRity` for `"dispRity"` `"randtest"` objects to output "ready-to-publish" result tables.
  * some error messages have been updated to be slightly more useful.
  * added the `estimation.details` argument to `multi.ace` allowing to also return specific arguments from the ancestral states estimation (thanks to [Armin Elsler](https://research-information.bris.ac.uk/en/persons/armin-elsler) for the suggestion).
  * Added new option `inc.nodes` to `clean.data` whether to check if the nodes in the tree match the labels in the matrix.
  * `make.metric` with the option `silent = TRUE` now outputs a list of info rather than only the level of the metric. You can reproduce the old behaviour using `make.metric(..., silent = TRUE)$type)`.
  * Fixed bug in `plot` using `preview` when the given argument `pch` did not match the number of groups (the different `pch` arguments are now used correctly).
  * Completely revamped the `ancestral.dist` metric. The function is now much faster and much easier to use (due to the new `dispRity` object structure). The options `nodes.coords` has been removed and the option `full` is now changed by `to.root`. If you still really want to use the older version of `ancestral.dist` using `ancestral.dist.deprecated` though.
  * The `dimensions` option throughout the package (e.g. in the `dispRity` function) can now also be a vector of dimensions to take into consideration (e.g. `c(1,2,5)`).
  * `chrono.subsets` now automatically detects the number of digits to round for the internal time slicing functions (thanks to [Mario Corio](https://mariocoiro.wordpress.com/) for finding this one).
  * Fixed bug in `test.metric` plots that now display correctly the "top" and "bottom" changes for the "position" shift.
  * Fixed bug in `test.metric` plots that now display the R^2 values correctly.
  * Fixed bug in `tree.age` when the tree tips/node labels vector is longer than the actual number of tips/nodes in the tree.
  * Removed former version of `ancestral.dist` (see NEW FEATURES above).
  * Removed `node.coordinates` function (no replacement; you must use a package version prior 1.5.10 to use this function).
  * Removed `get.ancestors` function (no replacement; you must use a package version prior 1.5.10 to use this function).

Previous patch notes and notes for the *next version* can be seen [here](https://github.com/TGuillerme/dispRity/blob/master/NEWS.md).

Authors
-------

* [Thomas Guillerme](http://tguillerme.github.io)
* [Natalie Cooper](http://nhcooper123.github.io)
* [Mark Puttick](https://puttickbiology.wordpress.com/)

#### Contributors (bug fixes, pull requests and suggestions)

[Dave Bapst](https://github.com/dwbapst/), [Mario Corio](https://mariocoiro.wordpress.com/), [Armin Elsler](https://research-information.bris.ac.uk/en/persons/armin-elsler), [Graeme Lloyd](http://graemetlloyd.com/) ([#104](https://github.com/TGuillerme/dispRity/pull/104)), [Jari Oksanen](https://github.com/jarioksa) ([#85](https://github.com/TGuillerme/dispRity/pull/85)), [Emmanuel Paradis](https://github.com/emmanuelparadis), [Abigail Pastore](https://github.com/aipastore), [Ashley Reaney](https://www.researchgate.net/profile/Ashley-Reaney), [Gavin Thomas](https://github.com/ghthomas).

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

If you use the `dispRity` pacakge for morphological disparity analyses, you should also check the excellent [`Claddis`](https://github.com/graemetlloyd/Claddis) package!


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

#### Disparity analyses jointly using the `dispRity` and [`Claddis`](https://github.com/graemetlloyd/Claddis) packages

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=The+patterns+and+modes+of+the+evolution+of+disparity+in+Mesozoic+birds&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Wang M, Lloyd GT, Zhang C, Zhou Z. The patterns and modes of the evolution of disparity in Mesozoic birds. Proceedings of the Royal Society B. **2021** [DOI:10;288(1944):20203105](https://royalsocietypublishing.org/doi/full/10.1098/rspb.2020.3105?casa_token=YSmPfapjEssAAAAA%3AYU3ya5sGZnwhtEkR5eP_UPUN7cJp8BR_HOoJ3vW3qfY_BUNI_FFXFrkc0-sVO5cl7iAaG8qpj4WLxA)
