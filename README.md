Release:

[![R-CMD-check](https://github.com/TGuillerme/dispRity/workflows/R-CMD-check/badge.svg)](https://github.com/TGuillerme/dispRity/actions)
[![codecov](https://codecov.io/gh/TGuillerme/dispRity/branch/release/graph/badge.svg)](https://codecov.io/gh/TGuillerme/dispRity)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![develVersion](https://img.shields.io/badge/devel%20version-1.8.0-green.svg?style=flat)](https://github.com/TGuillerme/dispRity)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1186467.svg)](https://doi.org/10.5281/zenodo.1186467)

Development (master):

[![R-CMD-check](https://github.com/TGuillerme/dispRity/workflows/R-CMD-check/badge.svg)](https://github.com/TGuillerme/dispRity/actions)
[![codecov](https://codecov.io/gh/TGuillerme/dispRity/branch/master/graph/badge.svg)](https://codecov.io/gh/TGuillerme/dispRity)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![dispRity status badge](https://phylotastic.r-universe.dev/badges/dispRity)](https://phylotastic.r-universe.dev)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1186467.svg)](https://doi.org/10.5281/zenodo.1186467)

CRAN:

[![minimal R version](https://img.shields.io/badge/R%3E%3D-4.0.0-6666ff.svg)](https://cran.r-project.org/)
<a href="https://CRAN.R-project.org/package=dispRity"><img src="https://www.r-pkg.org/badges/version/dispRity" alt="CRAN status"></a>
<a href="https://cran.r-project.org/package=dispRity"><img src="https://cranlogs.r-pkg.org/badges/grand-total/dispRity" alt="CRAN downloads"></a>

### **`dispRity`** is a `R` modular package for measuring disparity in multidimensional spaces.

<a href="https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/2041-210X.13022"><img src="http://tguillerme.github.io/images/OA.png" height="15" widht="15"/></a> 
Check out the [paper](https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/2041-210X.13022) associated with the first version of this package.

<a href="https://figshare.com/articles/New_approaches_to_disparity-through-time_analysis/3437546"><img src="http://tguillerme.github.io/images/logo-FS.png" height="15" widht="15"/></a> 
Check out the [presentation](https://figshare.com/articles/New_approaches_to_disparity-through-time_analysis/3437546) or the [video](https://www.youtube.com/watch?v=ZzipKw8W8KQ) of some of the package's features.

<!-- Link available until August 2019
https://programme.europa-organisation.com/slides/programme_jointCongressEvolBiology-2018/webconf/683_19082018_1140_antigone1_Thomas_Guillerme_480/index.html
 -->

## Installing dispRity

```r
install.packages("dispRity")
library(dispRity)
```

The package is also available in the [phylotastic r-universe](https://phylotastic.r-universe.dev/ui#packages) [![dispRity status badge](https://phylotastic.r-universe.dev/badges/dispRity)](https://phylotastic.r-universe.dev) or through the [phylogenetics CRAN Task View](https://cran.r-project.org/web/views/Phylogenetics.html). 


You can also install the piping hot development version (not always recommended!) by installing the package directly through github:

```r
if(!require(devtools)) install.packages("devtools")
library(devtools)
install_github("TGuillerme/dispRity")
library(dispRity)
```

See the [patch notes](https://github.com/TGuillerme/dispRity/blob/master/NEWS.md) for the latest developments.

## Vignettes and manuals

A detailed vignette is available [online](https://tguillerme.github.io/dispRity.html) or as a [pdf](https://github.com/TGuillerme/dispRity/blob/master/inst/gitbook/_book/dispRity_manual.pdf):

<a href="https://tguillerme.github.io/dispRity.html"><img src="http://tguillerme.github.io/images/rawgit.png" height="30"/></a> <a href="https://github.com/TGuillerme/dispRity/blob/master/inst/gitbook/_book/dispRity_manual.pdf"><img src="http://tguillerme.github.io/images/pdf.gif" height="30"/></a> 
 
Otherwise, each functions has a detailed associated manual with examples in `R` (`?which.function`).

Additionally, you can learn more about the structure of `dispRity` objects [here](https://github.com/TGuillerme/dispRity/blob/master/disparity_object.md).

### Workshop

You can download the workshop follow-along sheet [here](https://raw.githubusercontent.com/TGuillerme/dispRity/master/inst/vignettes/dispRity_workhop_code.Rmd) (use right click > save link as...) or visualise it in html [here](https://raw.githack.com/TGuillerme/dispRity/master/inst/vignettes/dispRity_workhop_code.html).

### Disparity/dissimilarity metrics/indices?

Not sure what disparity metric to use?
Not sure what a disparity metric is in the first place?
Check out this paper on selecting the best metric for your specific question in [Ecology and Evolution](https://onlinelibrary.wiley.com/doi/full/10.1002/ece3.6452) or the [`moms` shiny app](https://tguillerme.shinyapps.io/moms/).
You can also find more information in the [`dispRity` manual](https://rawcdn.githack.com/TGuillerme/dispRity/c94452e6877fbb274eb0a4ff1363272a6297a9ee/inst/gitbook/_book/details-of-specific-functions.html#disparity-metrics).

## Latest major patch highlights
### dispRity v1.8 (2023-12-11) *dispRity.multi*
<!-- ### dispRity v1.9 (2024-11-@@@) *distance update* -->
[Read the full patch note here](https://github.com/TGuillerme/dispRity/blob/master/NEWS.md).

 * Added the _dispRity.multi_ internal architecture now allowing users to work with different matrices **and** different trees as inputs for `custom.subsets`, `chrono.subsets`, `boot.matrix` and `dispRity`. This change is not affecting the user level appart from now allowing to bypass some error messages (thanks to Mario Corio for that obvious suggestion).
 * *New* statistical test: `pgls.dispRity` to run PGLS test on a `dispRity` object with a level-2 metric and a tree (using excellent [`phylolm`](https://CRAN.R-project.org/package=phylolm) algorithm). The new test comes with its own S3 print, summary and plot functions if the input `dispRity` data contains multiple trees or multiple matrices (running and handling the output of multiple `phylolm`).
 * *New vignette* compiling resources for developers to help people (and future me) to edit the package. 
 * And many more new additions, improvements and couple of bug fixes!
 * **NOTE** there are now changes in the following function names: `ellipse.volume` is now `ellipsoid.volume`; `rescale.dispRity` is now `scale.dispRity` and `randtest.dist` is now `distance.randtest` (the old aliases still work).

<!-- 
 * `dispRity` has been not greatly optimised for using distance matrices: 1) it's now much faster thanks to the `dist.helper` new optional argument (storing the distance matrix in the cache) and 2) it now allows direct analyses of distance matrices in a dispRity pipeline.
 * `boot.matrix` function has now been generalised to be able to bootstrap any elements of a matrix. Previously it only allowed to bootstrap elements (rows) but now can work on dimensions (columns) or both (distances).
 * Redesigned `multi.ace` to be more modular and better handle both continuous and/or discrete characters. This is secretly a pre-release for a future version that will greatly improve pipelines with ancestral state estimations ;).
 * New utility functions (`set.root.time` to add root times to trees; `remove.dispRity` to cleanly remove parts of dispRity objects) and metrics (`count.neigbhours`).
 * Loads of minor improvements and couple of bug fixes! Yay! -->

Previous patch notes and notes for the *next version* can be seen [here](https://github.com/TGuillerme/dispRity/blob/master/NEWS.md).

Authors
-------

* [Thomas Guillerme](http://tguillerme.github.io)
* [Natalie Cooper](http://nhcooper123.github.io)
* [Mark Puttick](https://puttickbiology.wordpress.com/)
* [Jack Hatfield](https://www.york.ac.uk/anthropocene-biodiversity/people/jack-hatfield/)

#### Contributors (bug fixes, pull requests and suggestions)

[Dave Bapst](https://github.com/dwbapst/), [Mario Corio](https://mariocoiro.wordpress.com/), [Armin Elsler](https://research-information.bris.ac.uk/en/persons/armin-elsler), [Graeme Lloyd](http://graemetlloyd.com/) ([Request #104](https://github.com/TGuillerme/dispRity/pull/104)), [Jari Oksanen](https://github.com/jarioksa) ([Request #85](https://github.com/TGuillerme/dispRity/pull/85)), [Emmanuel Paradis](https://github.com/emmanuelparadis), [Abigail Pastore](https://github.com/aipastore), [Ashley Reaney](https://www.researchgate.net/profile/Ashley-Reaney), [Gavin Thomas](https://github.com/ghthomas).

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
I have been developping this package while being hired succesively by these people (thanks a lot for supporting me develop this package, sometimes as a side project): [Natalie Cooper](http://nhcooper123.github.io/), [Martin Brazeau](http://www.imperial.ac.uk/people/m.brazeau), [Vera Weisbecker](https://www.flinders.edu.au/people/vera.weisbecker) and [Gavin Thomas](https://www.sheffield.ac.uk/biosciences/people/academic-staff/gavin-thomas).

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

#### Comparisons between groups using `custom.subsets` and `test.dispRity`:

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Phylogenomics%2C+biogeography+and+morphometrics+reveal+rapid+phenotypic+evolution+in+pythons+after+crossing+Wallace%27s+line&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Esquerré D, Donnellan S, Brennan IG, Lemmon AR, Lemmon EM, Zaher H, Grazziotin FG, Keogh JS. Phylogenomics, biogeography and morphometrics reveal rapid phenotypic evolution in pythons after crossing Wallace’s line. *Systematic Biology*. **2019** [DOI: 10.1093/sysbio/syaa024](https://doi.org/10.1093/sysbio/syaa024)

#### Bootstrapping data and comparing groups in ecospace using `boot.matrix`:

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&sciodt=0%2C5&cites=13311379491028410826&scipsc=&q=Morphological+discontinuous+variation+and+disparity+in+Lutzomyia+%28Tricholateralis%29+cruciata+Coquillett%2C+1907+are+not+related+to+contrasting+environmental+%E2%80%A6&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
de Oca-Aguilar AC, De Luna E, Rebollar-Téllez EA, Piermarini PM, Ibáñez-Bernal S. Morphological discontinuous variation and disparity in Lutzomyia (Tricholateralis) cruciata Coquillett, 1907 are not related to contrasting environmental factors in two biogeographical provinces. *Zoomorphology*. **2019** [DOI:10.1007/s00435-019-00450-8](https://link.springer.com/article/10.1007/s00435-019-00450-8)

#### Simulating disparity through time using `dtt.dispRity`:

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Ecological+interactions+shape+the+evolution+of+flower+color+in+communities+across+a+temperate+biodiversity+hotspot&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Skeels A, Dinnage R, Medina I, Cardillo M. Ecological interactions shape the evolution of flower color in communities across a temperate biodiversity hotspot *Evolution Letters* **2021** [10.1002/evl3.225](https://onlinelibrary.wiley.com/doi/full/10.1002/evl3.225)

#### Using the wrapper `disparity.per.group` function:

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Diet+variability+among+insular+populations+of+Podarcislizards+reveals+diverse+strategies+to+face+resource%E2%80%90limited+environments&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Taverne M, Fabre AC, King‐Gillies N, Krajnović M, Lisičić D, Martin L, Michal L, Petricioli D, Štambuk A, Tadić Z, Vigliotti C. Diet variability among insular populations of Podarcis lizards reveals diverse strategies to face resource‐limited environments. *Ecology and Evolution*. **2019** [DOI:10.1002/ece3.5626](https://onlinelibrary.wiley.com/doi/pdf/10.1002/ece3.5626).

#### Disparity analyses jointly using the `dispRity` and [`Claddis`](https://github.com/graemetlloyd/Claddis) packages

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=The+patterns+and+modes+of+the+evolution+of+disparity+in+Mesozoic+birds&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Wang M, Lloyd GT, Zhang C, Zhou Z. The patterns and modes of the evolution of disparity in Mesozoic birds. Proceedings of the Royal Society B. **2021** [DOI:10;288(1944):20203105](https://royalsocietypublishing.org/doi/full/10.1098/rspb.2020.3105?casa_token=YSmPfapjEssAAAAA%3AYU3ya5sGZnwhtEkR5eP_UPUN7cJp8BR_HOoJ3vW3qfY_BUNI_FFXFrkc0-sVO5cl7iAaG8qpj4WLxA)
