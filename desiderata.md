# To do list

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
