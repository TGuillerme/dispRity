Release:

[![Build Status](https://travis-ci.org/TGuillerme/dispRity.svg?branch=release)](https://travis-ci.org/TGuillerme/dispRity)
[![codecov](https://codecov.io/gh/TGuillerme/dispRity/branch/release/graph/badge.svg)](https://codecov.io/gh/TGuillerme/dispRity)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![develVersion](https://img.shields.io/badge/devel%20version-1.3.1-green.svg?style=flat)](https://github.com/TGuillerme/dispRity/tree/release)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1186467.svg)](https://doi.org/10.5281/zenodo.1186467)


Development (master):

[![Build Status](https://travis-ci.org/TGuillerme/dispRity.svg?branch=master)](https://travis-ci.org/TGuillerme/dispRity)
[![codecov](https://codecov.io/gh/TGuillerme/dispRity/branch/master/graph/badge.svg)](https://codecov.io/gh/TGuillerme/dispRity)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![develVersion](https://img.shields.io/badge/devel%20version-1.3.1-green.svg?style=flat)](https://github.com/TGuillerme/dispRity)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1186467.svg)](https://doi.org/10.5281/zenodo.1186467)


CRAN:

[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.3.3-6666ff.svg)](https://cran.r-project.org/)
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

[How do I know which version to choose?](https://rawgit.com/TGuillerme/dispRity/master/inst/gitbook/_book/index.html#version)

## Vignettes and manuals

A detailed vignette is available [online](https://tguillerme.github.io/dispRity.html) or as a [pdf](https://github.com/TGuillerme/dispRity/blob/master/inst/gitbook/_book/dispRity_manual.pdf):

 <a href="https://tguillerme.github.io/dispRity.html"><img src="http://tguillerme.github.io/images/rawgit.png" height="30"/></a> <a href="https://github.com/TGuillerme/dispRity/blob/master/inst/gitbook/_book/dispRity_manual.pdf"><img src="http://tguillerme.github.io/images/pdf.gif" height="30"/></a> 
 
Otherwise, each functions has a detailed associated manual with examples in `R` (`?which.function`).

Additionally, you can learn more about the structure of `dispRity` objects [here](https://github.com/TGuillerme/dispRity/blob/master/disparity_object.md).

## Latest patch notes
* 2018/08/15 v1.3 *many metrics*

    * *New* metric: `displacements`, a dimension level 2 metric that measures the position of elements in space (i.e. their distance from the centre relative to their distance to their centroid).
    * *New* metric: `neighbours`, a dimension level 2 metric that measures the distance from an element to its neighbour (e.g. the nearest neighbour, the furthest, the median, etc.).
    * *New* metric: `quantiles`, a dimension level 2 metric that measures the *n*th quantile range per axis (a good alternative to the `ranges` function!).
    * *New* metric: `func.eve`, a dimension level 1 metric that measures the functional evenness (i.e. the spread along the minimum spanning tree; from Villéger et al. 2008).
    * *New* metric: `func.div`, a dimension level 1 metric that measures the functional divergence (i.e. the ratio of deviation from the centroid; from Villéger et al. 2008).
    * *Updated* metric: `span.tree.length` now outputs the length of each edges (c.f. the sum of the length) and becomes a level 2 metric.
    * The `chrono.subsets` can now take `multiPhylo` objects for slicing through multiple trees at once!
    * *New* utility function: `reduce.matrix` for optimising data overlap in a matrix with missing data.
    * *New* utility function: `slide.nodes` for sliding specific nodes on a tree.
    * *New* utility function: `remove.zero.brlen` for stochastically removing zero branch lengths on a tree (using the `slide.nodes` function).
    * **New argument** in `plot.dispRity`: the `type` argument can now be `"preview"` to have a glimpse at two of the dimensions of the trait-space.
    * The `Claddis.ordination` can now directly take a matrix's path as input (leaving the function to read and transform the matrix into `Claddis` format. The function can thus now also be used to convert matrices into `Claddis` format.
    * Added a "Other functionalities" section to the manual describing miscellaneous functions.
    * `centroids` and `ancestral.dist` functions can now take the `method` option for `"euclidean"` or `"manhattan"` distances.
    * All functions methods selection have now been sped up using `switch`.
    * Error messages in `dispRity` are more verbose when input the wrong metric(s).
    * `scree` option in `space.maker` does not require to sum up to one anymore.
    * `cor.matrix` option in `space.maker` does not require to have a valid Choleski decomposition (an approximation is used instead).
    * Updated all tests and functions to be compatible with R 3.6.
    * Fixed bug in `clean.data` that did not output dropped tips correctly when applied on `multiPhylo` objects.
    * Improved error messages in `chrono.subsets` for funky time slices/bins (e.g. with negative values).
    * Speed improvements for the `time.slice` function.
    * Better internal handling of distance matrices for the disparity metrics.
    * Most functions handles `NA` as `na.rm` or `na.omit`.

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

To cite the [`dispRity` manual](https://rawgit.com/TGuillerme/dispRity/master/inst/gitbook/_book/dispRity_manual.pdf), please use:

* Guillerme, T. & Cooper, N. (**2018**) dispRity manual. *figshare*. Preprint. 10.6084/m9.figshare.6187337.v1
    ##### [BibTex](https://figshare.com/articles/6187337/1/citations/bibtex), [EndNote](https://figshare.com/articles/6187337/1/citations/endnote), [RefWorks](https://figshare.com/articles/6187337/1/citations/refworks), [DataCite](https://figshare.com/articles/6187337/1/citations/datacite), [more...](https://figshare.com/articles/dispRity_manual/6187337)

To cite the [time slicing method](https://onlinelibrary.wiley.com/doi/abs/10.1111/pala.12364) from the `chrono.subsets` function, please use:

* Guillerme, T. & Cooper, N. (**2018**) Time for a rethink: time sub‐sampling methods in disparity‐through‐time analyses. *Palaeontology*, 61: 481-493. [doi:10.1111/pala.12364](https://onlinelibrary.wiley.com/doi/abs/10.1111/pala.12364)
    ##### [Export citation](https://onlinelibrary.wiley.com/action/showCitFormats?doi=10.1111%2Fpala.12364)

Acknowledgments
-------
Some ideas/functionalities/implementations in this package where implemented following the suggestions of [Natalie Cooper](http://nhcooper123.github.io/), [Graeme Lloyd](http://www.graemetlloyd.com/), [Dave Bapst](https://github.com/dwbapst/), [Andrew Jackson](https://www.tcd.ie/Zoology/people/jacksoan) and [Martin Brazeau](http://www.imperial.ac.uk/people/m.brazeau).


Used in
-------

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Diet+variability+among+insular+populations+of+Podarcislizards+reveals+diverse+strategies+to+face+resource%E2%80%90limited+environments&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
M Taverne, AC Fabre, N King‐Gillies, M Krajnović, D Lisičić, L Martin, L Michal, D Petricioli, A Štambuk, Z Tadić, C Vigliotti. (**2019**) Diet variability among insular populations of Podarcis lizards reveals diverse strategies to face resource‐limited environments. Ecology and Evolution. [DOI:10.1002/ece3.5626](https://onlinelibrary.wiley.com/doi/pdf/10.1002/ece3.5626).


* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Oceanic+islands+of+Wallacea+as+a+source+for+dispersal+and+diversification+of+murine+rodents&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
KC Rowe, AS Achmadi, PH Fabre, JJ Schenk, SJ Steppan, JA Esselstyn. (**2019**) Oceanic islands of Wallacea as a source for dispersal and diversification of murine rodents. Journal of Biogeography.[DOI:10.1007/s11692-019-09482-w](https://link.springer.com/article/10.1007/s11692-019-09482-w)


* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=The+Shape+of+Weaver%3A+Investigating+Shape+Disparity+in+Orb-Weaving+Spiders+%28Araneae%2C+Araneidae%29+Using+Geometric+Morphometrics&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
RJ Kallal, Aj Moore, G Hormiga. (**2019**) The Shape of Weaver: Investigating Shape Disparity in Orb-Weaving Spiders (Araneae, Araneidae) Using Geometric Morphometrics. *Evolutionary Biology*. 2019:1-5.[DOI:10.1007/s11692-019-09482-w](https://link.springer.com/article/10.1007/s11692-019-09482-w)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Crocodylomorph+cranial+shape+evolution+and+its+relationship+with+body+size+and+ecology&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
PL Godoy (**2019**). Crocodylomorph cranial shape evolution and its relationship with body size and ecology. *bioRxiv*. 1:640383. [DOI:10.1101/724609 ](https://www.biorxiv.org/content/10.1101/724609v1.full)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=A+Kuramoto+model+of+self-other+integration+across+interpersonal+synchronization+strategies&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
OA Heggli, J Cabral, I Konvalinka, P Vuust, ML Kringelbach (**2019**). A Kuramoto model of self-other integration across interpersonal synchronization strategies. *bioRxiv*. 1:724609. [DOI:10.1101/640383](https://www.biorxiv.org/content/10.1101/640383v1.full)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&sciodt=0%2C5&cites=13311379491028410826&scipsc=&q=Morphological+discontinuous+variation+and+disparity+in+Lutzomyia+%28Tricholateralis%29+cruciata+Coquillett%2C+1907+are+not+related+to+contrasting+environmental+%E2%80%A6&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
AC de Oca-Aguilar, E De Luna, EA Rebollar-Téllez, PM Piermarini, S Ibáñez-Bernal. (**2019**) Morphological discontinuous variation and disparity in Lutzomyia (Tricholateralis) cruciata Coquillett, 1907 are not related to contrasting environmental factors in two biogeographical provinces. *Zoomorphology*. 2019:1-4. [DOI:10.1007/s00435-019-00450-8](https://link.springer.com/article/10.1007/s00435-019-00450-8)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&sciodt=0%2C5&cites=13311379491028410826&scipsc=&q=Fossils+reveal+long-term+continuous+and+parallel+innovation+in+the+sacro-caudo-pelvic+complex+of+the+highly+aquatic+pipid+frogs&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
RO Gómez, CM Pérez-Ben. (**2019**) Fossils Reveal Long-Term Continuous and Parallel Innovation in the Sacro-Caudo-Pelvic Complex of the Highly Aquatic Pipid Frogs. *Frontiers in Earth Science* [DOI: 10.3389/feart.2019.00056](https://www.frontiersin.org/articles/10.3389/feart.2019.00056/full)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Diversity+and+Disparity+of+therocephalia%3A+Macroevolutionary+patterns+through+two+Mass+extinctions&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
HR Grunert, N Brocklehurst, J Fröbisch. (**2019**) Diversity and Disparity of therocephalia: Macroevolutionary patterns through two Mass extinctions. *Scientific Reports* [DOI: 10.1038/s41598-019-41628-w](https://www.nature.com/articles/s41598-019-41628-w#ref-CR55)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Does+exceptional+preservation+distort+our+view+of+disparity+in+the+fossil+record%3F+&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
JT Flannery Sutherland, BC Moon, TL Stubbs, MJ Benton. (**2019**) Does exceptional preservation distort our view of disparity in the fossil record? *Proceedings of the Royal Society B* [DOI: 10.1098/rspb.2019.0091](https://royalsocietypublishing.org/doi/10.1098/rspb.2019.0091#d164727e1)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&sciodt=0%2C5&cites=13311379491028410826&scipsc=&q=Speciation+Rate+Is+Independent+of+the+Rate+of+Evolution+of+Morphological+Size%2C+Shape%2C+and+Absolute+Morphological+Specialization+in+a+Large+Clade+of+Birds&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
NMA Crouch, RE Ricklefs. (**2019**)Speciation Rate Is Independent of the Rate of Evolution of Morphological Size, Shape, and Absolute Morphological Specialization in a Large Clade of Birds *American Naturalist* [DOI: 10.1086/701630](https://www.journals.uchicago.edu/doi/abs/10.1086/701630)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&sciodt=0%2C5&cites=693693665572195425&scipsc=&q=Morphology+and+stable+isotope+analysis+demonstrate+different+structuring+of+bat+communities+in+rainforest+and+savannah+habitats&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
A Monadjem, A Kane, P Taylor, LR Richards, G Hall, S Woodborne. (**2018**) Morphology and stable isotope analysis demonstrate different structuring of bat communities in rainforest and savannah habitats. *Royal Society Open Science* [DOI: 10.1098/rsos.180849](https://royalsocietypublishing.org/doi/full/10.1098/rsos.180849)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Phylogeny%2C+macroevolutionary+trends+and+historical+biogeography+of+sloths%3A+insights+from+a+Bayesian+morphological+clock+analys&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
L Varela, PS Tambusso, HG McDonald, RA Fariña (**2018**) Phylogeny, macroevolutionary trends and historical biogeography of sloths: insights from a Bayesian morphological clock analysis. *Systematic Biology*. [DOI: 10.1093/sysbio/syy058](https://academic.oup.com/sysbio/advance-article/doi/10.1093/sysbio/syy058/5098296)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Whole-Genome+Duplication+and+Plant+Macroevolution&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
JW Clark, PCJ Donoghue (**2018**) Whole-Genome Duplication and Plant Macroevolution. *Trends in plant science*. [DOI: 10.1016/j.tplants.2018.07.006](https://www.sciencedirect.com/science/article/pii/S1360138518301596)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&sciodt=0%2C5&cites=693693665572195425&scipsc=&q=The+long-term+ecology+and+evolution+of+marine+reptiles+in+a+Jurassic+seaway&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
D Foffa, MT Young, TL Stubbs, KG Dexter, SL Brusatte (**2018**) The long-term ecology and evolution of marine reptiles in a Jurassic seaway. *Nature Ecology & Evolution*. [DOI: 10.1038/s41559-018-0656-6](https://www.nature.com/articles/s41559-018-0656-6)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Time+for+a+rethink%3A+time+sub%E2%80%90sampling+methods+in+disparity%E2%80%90through%E2%80%90time+analyses&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
T Guillerme, N Cooper (**2018**) Time for a rethink: time sub‐sampling methods in disparity‐through‐time analyses. *Palaeontology*. 61: 481-493. [DOI: 10.1111/pala.12364](https://onlinelibrary.wiley.com/doi/abs/10.1111/pala.12364)
