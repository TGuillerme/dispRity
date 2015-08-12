# dispRity
A modulable package for measuring disparity in R


**dispRity** is a colaborative and modulable `R` package for measuring disparity from ordinated data (PCA, PCO, PCoA, MDS, etc).

#### [Implemented metric](https://github.com/TGuillerme/dispRity/blob/master/metrics.md)
Or soon to be implemented...

##Installing dispRity
#### DISCLAIMER: the package is in early developement stage (no release yet)
###### You can install it following the instruction below but it's likely to crash.
```r
#install.packages("devtools")
library(devtools)
install_github("TGuillerme/disRity", ref="release")
library(dispRity)
```
The following installs the latest released version (see patch notes below). For the piping hot development version (not recommended), replace the `ref="release"` option by `ref="master"`.


##Developer
###### Remove this section after first release
Little tip for smoothing the package developement (run pass test):
```r
refresh.dispRity<-function(){
library(devtools)
setwd('~/Packaging/')
install('dispRity')
library(dispRity)}
```
Make sure you change `setwd('~/Packaging/')` to the actual path. Note that this function is probably useless for most people.
