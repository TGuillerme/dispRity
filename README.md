Release:

[![Build Status](https://travis-ci.org/TGuillerme/dispRity.svg?branch=release)](https://travis-ci.org/TGuillerme/dispRity)
[![codecov](https://codecov.io/gh/TGuillerme/dispRity/branch/release/graph/badge.svg)](https://codecov.io/gh/TGuillerme/dispRity)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![develVersion](https://img.shields.io/badge/devel%20version-1.4-green.svg?style=flat)](https://github.com/TGuillerme/dispRity/tree/release)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1186467.svg)](https://doi.org/10.5281/zenodo.1186467)


Development (master):

[![Build Status](https://travis-ci.org/TGuillerme/dispRity.svg?branch=master)](https://travis-ci.org/TGuillerme/dispRity)
[![codecov](https://codecov.io/gh/TGuillerme/dispRity/branch/master/graph/badge.svg)](https://codecov.io/gh/TGuillerme/dispRity)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![develVersion](https://img.shields.io/badge/devel%20version-1.4-green.svg?style=flat)](https://github.com/TGuillerme/dispRity)
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

### Disparity/dissimilarity metrics/indices?

Not sure what disparity metric to use?
Not sure what a disparity metric is in the first place?
Check out this pre-print on selecting the best metric for your specific question on [biorXiv](https://www.biorxiv.org/content/10.1101/801571v1) or the [`moms` shiny app](https://tguillerme.shinyapps.io/moms/).
You can also find more information in the [`dispRity` manual](https://rawcdn.githack.com/TGuillerme/dispRity/c94452e6877fbb274eb0a4ff1363272a6297a9ee/inst/gitbook/_book/details-of-specific-functions.html#disparity-metrics).
<!-- biorXiv preprint -->

## Latest patch notes
* 2020/05/05 v1.4 *getting faster*

  * *New* metric: `angles`, a dimension level 2 metric that measures the angle of the main axis of each dimension in a matrix (in slopes, angles or degrees).
  * *New* metric: `deviations`, a dimension level 2 metric that measures the deviation of each element from a hyperplane.
  * Completely rewritten `char.diff` function. It now uses a way faster bitwise comparison architecture and comes with different distance methods as well as modular optional arguments on how to treat various special tokens (`"?"`, `"-"`, `"&"`, etc.). This also allows many more distance methods (now including `"hamming"`, `"manhattan"`, `"comparable"`, `"euclidean"`, `"maximum"` and `"mord"`).
  * all `dispRity` functions can now intake a single `"matrix"` or a `"list"` of matrices with the same row names and dimensions. The disparity is then calculated directly on all the matrices and summarised as before through `summary.dispRity`. This option can be used to add uncertainty to disparity calculations. For example in `chrono.subsets` you can now provide a list of trees and a list of associated ancestral state estimates; or for `custom.subsets` you can provide a list of matrices with different values representing different estimations of the traits.
  * update `reduce.matrix` to work with `vegan::vegdist` version 2.5-6 (thanks to [Jari Oksanen for the fix](https://github.com/TGuillerme/dispRity/pull/85)).
  * updated class evaluations throughout the package for `R` version `4.0.0`: `class(.) == *` is now `is(., *)`.
  * updated `...` argument bug PR#16223.
  * In `make.metric` the argument `...` is now ignored if any `names(...)` is `"tree"` or `"phy"`.
  * fixed bug in `neighbours` and `span.tree.length` when feeding "distance" like metrics (thanks to Ashley Reaney for finding that one).
  * greatly improved speed of `chrono.subsets` with `method = "continuous` (now > 1000 times faster!).
  * minor warning message fix for `plot.dispRity` with time slices.
  * removed `paleotree` package dependency (though the links to this excellent package are still there!).
  * increased `R` version requirement to `3.5`.
  * `...` in `summary.dispRity` are now directly passed to `cent.tend` (e.g. `na.rm = TRUE`).
  * added some time improvements in several phylo functions based on the `castor` package.
  * updated all the package demo data to fit the new `dispRity` object architecture (see above). Note that this might effect the exact results of calculations using these demo datasets.
  * you can now specify the dimensions of the matrix to make a disparity metric in `make.metric` through the `data.dim` option. 
  * metrics passed in `dispRity` are now tested using the input data dimensions.
  * `chrono.subsets` with multiple trees now stretches the root edges length to match the oldest tree.

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
<!-- > 25 publications have used `dispRity` since 04/2018 (12.5 per year). (calculated on the 03/04/2020). -->

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Categorical+versus+geometric+morphometric+approaches+to+characterizing+the+evolution+of+morphological+disparity+in+Osteostraci+%28Vertebrata%2C+stem+Gnathostomata%29&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Ferron HG, Greenwood JM, Deline B, Martinez-Perez C, Botella H, Sansom RS, Ruta M, Donoghue PC. Categorical versus geometric morphometric approaches to characterizing the evolution of morphological disparity in Osteostraci (Vertebrata, stem Gnathostomata). *Palaeontology*. **2020** [DOI: 10.1111/pala.12482](https://sci-hub.se/10.1111/pala.12482)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=+Megaevolutionary+dynamics+in+reptiles+and+the+role+of+adaptive+radiations+in+evolutionary+innovation&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Simoes TR, Vernygora O, Caldwell MW, Pierce SE. Megaevolutionary dynamics in reptiles and the role of adaptive radiations in evolutionary innovation. *bioRxiv*. **2020** [DOI: 10.1101/2020.04.22.055061](https://doi.org/10.1101/2020.04.22.055061)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Intraspecific+variation+in+the+cochleae+of+harbour+porpoises+%28Phocoena+phocoena%29+and+its+implications+for+comparative+studies+across+odontocetes&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Martins MC, Park T, Racicot R, Cooper N. Intraspecific variation in the cochleae of harbour porpoises (Phocoena phocoena) and its implications for comparative studies across odontocetes. *PeerJ*. **2020** [DOI: 10.7717/peerj.8916](https://peerj.com/articles/8916/)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Shoulder+Muscle+Architecture+in+the+Echidna+%28Monotremata%3A+Tachyglossus+aculeatus%29+Indicates+Conserved+Functional+Properties&btnG=#d=gs_cit&u=%2Fscholar%3Fq%3Dinfo%3A2T425Bdh8p4J%3Ascholar.google.com%2F%26output%3Dcite%26scirp%3D0%26hl%3Den"><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Regnault S, Fahn-Lai P, Norris RM, Pierce SE. Shoulder Muscle Architecture in the Echidna (Monotremata: Tachyglossus aculeatus) Indicates Conserved Functional Properties. *Journal of Mammalian Evolution*. **2020** [DOI: 10.1007/s10914-020-09498-6](https://link.springer.com/article/10.1007/s10914-020-09498-6)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&sciodt=0%2C5&as_ylo=2020&cites=13311379491028410826&scipsc=&q=Early+high+rates+and+disparity+in+the+evolution+of+ichthyosaurs&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Moon BC, Stubbs TL. Early high rates and disparity in the evolution of ichthyosaurs. *Communications biology*. **2020** [DOI: 10.1038/s42003-020-0779-6](https://www.nature.com/articles/s42003-020-0779-6)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Crocodylomorph+cranial+shape+evolution+and+its+relationship+with+body+size+and+ecology&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Godoy PL. Crocodylomorph cranial shape evolution and its relationship with body size and ecology. *Journal of Evolutionary Biology*. **2020** [DOI: 10.1111/jeb.13540](https://onlinelibrary.wiley.com/doi/full/10.1111/jeb.13540)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&sciodt=0%2C5&as_ylo=2020&cites=13311379491028410826&scipsc=&q=Otolith+morphological+divergences+of+successful+Lessepsian+fishes+on+the+Mediterranean+coastal+waters&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Tuset VM, Lombarte A, Bariche M, Maynou F, Azzurro E. Otolith morphological divergences of successful Lessepsian fishes on the Mediterranean coastal waters. *Estuarine, Coastal and Shelf Science*. **2020** [DOI: 10.1016/j.ecss.2020.106631](https://www.sciencedirect.com/science/article/pii/S0272771419311291)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Phylogenomics%2C+biogeography+and+morphometrics+reveal+rapid+phenotypic+evolution+in+pythons+after+crossing+Wallace%27s+line&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Esquerré D, Donnellan S, Brennan IG, Lemmon AR, Lemmon EM, Zaher H, Grazziotin FG, Keogh JS. Phylogenomics, biogeography and morphometrics reveal rapid phenotypic evolution in pythons after crossing Wallace’s line. *Systematic Biology*. **2019** [DOI: 10.1093/sysbio/syaa024](https://doi.org/10.1093/sysbio/syaa024)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Morphological+disparity+in+theropod+jaws%3A+comparing+discrete+characters+and+geometric+morphometrics&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Schaeffer J, Benton MJ, Rayfield EJ, Stubbs TL. Morphological disparity in theropod jaws: comparing discrete characters and geometric morphometrics. *Palaeontology*. **2019** [DOI: 10.1111/pala.12455](https://onlinelibrary.wiley.com/doi/full/10.1111/pala.12455)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Origin+of+horsetails+and+the+role+of+whole-genome+duplication+in+plant+macroevolution&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Clark JW, Puttick MN, Donoghue PC. Origin of horsetails and the role of whole-genome duplication in plant macroevolution. *Proceedings of the Royal Society B*. **2019** [DOI: 10.1098/rspb.2019.1662](https://royalsocietypublishing.org/doi/full/10.1098/rspb.2019.1662)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Individual+variation+of+the+masticatory+system+dominates+3D+skull+shape+in+the+herbivory-adapted+marsupial+wombats&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Weisbecker V, Guillerme T, Speck C, Sherratt E, Abraha HM, Sharp AC, Terhune CE, Collins S, Johnston S, Panagiotopoulou O. Individual variation of the masticatory system dominates 3D skull shape in the herbivory-adapted marsupial wombats. *Frontiers in zoology*. **2019** [DOI: 10.1186/s12983-019-0338-5](https://frontiersinzoology.biomedcentral.com/articles/10.1186/s12983-019-0338-5)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Shifting+spaces%3A+which+disparity+or+dissimilarity+metrics+best+summarise+occupancy+in+multidimensional+spaces%3F&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Guillerme T, Puttick MN, Marcy AE, Weisbecker V. Shifting spaces: which disparity or dissimilarity metrics best summarise occupancy in multidimensional spaces?. *BioRxiv*. **2019** [DOI: 10.1101/801571 ](https://www.biorxiv.org/content/10.1101/801571v1).

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=High+ecomorphological+diversity+among+Early+Cretaceous+frogs+from+a+large+subtropical+wetland+of+Iberia&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Gómez RO, Lires AI. High ecomorphological diversity among Early Cretaceous frogs from a large subtropical wetland of Iberia. *Comptes Rendus Palevol*. **2019** [DOI: 10.1016/j.crpv.2019.07.005](https://www.sciencedirect.com/science/article/pii/S1631068319301320).

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Diet+variability+among+insular+populations+of+Podarcislizards+reveals+diverse+strategies+to+face+resource%E2%80%90limited+environments&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Taverne M, Fabre AC, King‐Gillies N, Krajnović M, Lisičić D, Martin L, Michal L, Petricioli D, Štambuk A, Tadić Z, Vigliotti C. Diet variability among insular populations of Podarcis lizards reveals diverse strategies to face resource‐limited environments. *Ecology and Evolution*. **2019** [DOI:10.1002/ece3.5626](https://onlinelibrary.wiley.com/doi/pdf/10.1002/ece3.5626).

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=The+Shape+of+Weaver%3A+Investigating+Shape+Disparity+in+Orb-Weaving+Spiders+%28Araneae%2C+Araneidae%29+Using+Geometric+Morphometrics&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Kallal RJ, Moore AJ, Hormiga G. The Shape of Weaver: Investigating Shape Disparity in Orb-Weaving Spiders (Araneae, Araneidae) Using Geometric Morphometrics. *Evolutionary Biology*. **2019** [DOI:10.1007/s11692-019-09482-w](https://link.springer.com/article/10.1007/s11692-019-09482-w)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=A+Kuramoto+model+of+self-other+integration+across+interpersonal+synchronization+strategies&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Heggli OA, Cabral J, Konvalinka I, Vuust P, Kringelbach ML. A Kuramoto model of self-other integration across interpersonal synchronization strategies. *PLoS computational biology*. **2019** [DOI:10.1371/journal.pcbi.1007422](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1007422)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&sciodt=0%2C5&cites=13311379491028410826&scipsc=&q=Morphological+discontinuous+variation+and+disparity+in+Lutzomyia+%28Tricholateralis%29+cruciata+Coquillett%2C+1907+are+not+related+to+contrasting+environmental+%E2%80%A6&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
de Oca-Aguilar AC, De Luna E, Rebollar-Téllez EA, Piermarini PM, Ibáñez-Bernal S. Morphological discontinuous variation and disparity in Lutzomyia (Tricholateralis) cruciata Coquillett, 1907 are not related to contrasting environmental factors in two biogeographical provinces. *Zoomorphology*. **2019** [DOI:10.1007/s00435-019-00450-8](https://link.springer.com/article/10.1007/s00435-019-00450-8)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&sciodt=0%2C5&cites=13311379491028410826&scipsc=&q=Fossils+reveal+long-term+continuous+and+parallel+innovation+in+the+sacro-caudo-pelvic+complex+of+the+highly+aquatic+pipid+frogs&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Gómez RO, Pérez-Ben CM. Fossils reveal long-term continuous and parallel innovation in the sacro-caudo-pelvic complex of the highly aquatic pipid frogs. *Frontiers in Earth Science*. **2019** [DOI: 10.3389/feart.2019.00056](https://www.frontiersin.org/articles/10.3389/feart.2019.00056/full)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Diversity+and+Disparity+of+therocephalia%3A+Macroevolutionary+patterns+through+two+Mass+extinctions&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Grunert HR, Brocklehurst N, Fröbisch J. Diversity and Disparity of therocephalia: Macroevolutionary patterns through two Mass extinctions. *Scientific reports*. **2019** [DOI: 10.1038/s41598-019-41628-w](https://www.nature.com/articles/s41598-019-41628-w#ref-CR55)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Does+exceptional+preservation+distort+our+view+of+disparity+in+the+fossil+record%3F+&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Flannery Sutherland JT, Moon BC, Stubbs TL, Benton MJ. Does exceptional preservation distort our view of disparity in the fossil record?. *Proceedings of the Royal Society B*. **2019** [DOI: 10.1098/rspb.2019.0091](https://royalsocietypublishing.org/doi/10.1098/rspb.2019.0091#d164727e1)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&sciodt=0%2C5&cites=13311379491028410826&scipsc=&q=Speciation+Rate+Is+Independent+of+the+Rate+of+Evolution+of+Morphological+Size%2C+Shape%2C+and+Absolute+Morphological+Specialization+in+a+Large+Clade+of+Birds&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Crouch NM, Ricklefs RE. Speciation rate is independent of the rate of evolution of morphological size, shape, and absolute morphological specialization in a large clade of birds. *The American Naturalist*. **2019** [DOI: 10.1086/701630](https://www.journals.uchicago.edu/doi/abs/10.1086/701630)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=The+long-term+ecology+and+evolution+of+marine+reptiles+in+a+Jurassic+seaway&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Foffa D, Young MT, Stubbs TL, Dexter KG, Brusatte SL. The long-term ecology and evolution of marine reptiles in a Jurassic seaway. *Nature ecology & evolution*. **2018** [DOI: 10.1038/s41559-018-0656-6](https://www.nature.com/articles/s41559-018-0656-6)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&sciodt=0%2C5&cites=693693665572195425&scipsc=&q=Morphology+and+stable+isotope+analysis+demonstrate+different+structuring+of+bat+communities+in+rainforest+and+savannah+habitats&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a> 
Monadjem A, Kane A, Taylor P, Richards LR, Hall G, Woodborne S. Morphology and stable isotope analysis demonstrate different structuring of bat communities in rainforest and savannah habitats. *Royal Society open science*. **2018** [DOI: 10.1098/rsos.180849](https://royalsocietypublishing.org/doi/full/10.1098/rsos.180849)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Phylogeny%2C+macroevolutionary+trends+and+historical+biogeography+of+sloths%3A+insights+from+a+Bayesian+morphological+clock+analys&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Varela L, Tambusso PS, McDonald HG, Fariña RA. Phylogeny, macroevolutionary trends and historical biogeography of sloths: insights from a Bayesian morphological clock analysis. *Systematic biology*. **2019** [DOI: 10.1093/sysbio/syy058](https://academic.oup.com/sysbio/advance-article/doi/10.1093/sysbio/syy058/5098296)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Whole-Genome+Duplication+and+Plant+Macroevolution&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Clark JW, Donoghue PC. Whole-genome duplication and plant macroevolution. *Trends in plant science*. **2018** [DOI: 10.1016/j.tplants.2018.07.006](https://www.sciencedirect.com/science/article/pii/S1360138518301596)

* <a href="https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Time+for+a+rethink%3A+time+sub%E2%80%90sampling+methods+in+disparity%E2%80%90through%E2%80%90time+analyses&btnG="><img src="http://tguillerme.github.io/images/649298-64.png" height="15" widht="15"/></a>
Guillerme T, Cooper N. Time for a rethink: time sub‐sampling methods in disparity‐through‐time analyses. *Palaeontology*. **2018** [DOI: 10.1111/pala.12364](https://onlinelibrary.wiley.com/doi/abs/10.1111/pala.12364)
