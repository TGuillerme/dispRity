# Package wish list
* Allow disparity metrics that work on a full distribution (i.e. for then allowing to compare the observed distributions directly)

# To do list
* Add `global` and `obs` metrics modifiers: by default disparity metrics are calculated on the `dispRity` object's series. These two modifiers allow the metrics to be calculated on the observed disparity or the global disparity.
For example `dispRity(data, metric = c(median, obs(centroids)))` allows to calculate the median distance between each series's element and the observed overall centroid of the ordinated space.
* Add a `k.root` function for scaling at the **k**th root where **k** is the number of ordination axis (useful for scaling product metrics).
* Allow more level 1 functions for metric (e.g. `metric = c(k.root, prod, variances)`.


## Literature review
* Collect more metrics!

## Package
* develop a parallel version using `snow` package. New options would be:
  * `cluster`, default= `FALSE`, else a vector contianing the number of nodes, the parallel type and any supplementary options (e.g. `cluster=c(2,"MPI")`)
* Add testing function
* Allow `cust.series` to deal with more than one factor column
* Implemented `make` class functions (`make.metric`, `make.model`, `make.boot`) 
* Fix verbose typos
* Implement coverage approach for rarefactions (see Kotric and Knoll in Paleobiology)
* Allow `print.dispRity` to print the full list of lists.
* Do not limit `dispRity` to two metric functions: allow any number of metrics to be used.

## Testing the metrics
Which metric is best with:
* missing data
* sample size
* taxonomic diversity (k)
* phylogenetic diversity (branch length)

### Comparisons

   | axis variance | axis range | distance from centroid | intertaxa distance |
---|------|------|---------|----------|
mean | | | | |
median | | | | |
sum | | | | |
product | | | | |
variance | | | | |
range | | | | |
