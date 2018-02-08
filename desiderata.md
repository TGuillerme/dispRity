# Package wish list
* Add the possibility to use a `multiPhylo` object in `time.series`.
* Improve `test.dispRity` architecture.
* Change the structure of the `dispRity` calculation loop to be run in `C`. `make.metric` would then be a compiler function.
* Add "clever" rarefaction algorithms: 1) http://www.bioone.org/doi/full/10.1017/pab.2014.5 and 2) with rarefaction as a function of sampling or occurrences.
* Add correlation with sample size for disparity metrics (calculate Spearman’s rho for the correlation between the metric and the sample size).

 * Add distributions for FADLADs + FADLADs of nodes