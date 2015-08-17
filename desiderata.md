# To do list

## Literature review
* Collect more metrics!

## Package
* Add a blocker in the sanitizing for the input data: must be a matrix k*<=k-1
* Add manuals!
* develop a parallel version using `snow` package. New options would be:
  * `cluster`, default= `FALSE`, else a vector contianing the number of nodes, the parallel type and any supplementary options (e.g. `cluster=c(2,"MPI")`)

## Testing the metrics
Which metric is best with:
* missing data
* sample size
* taxonomic diversity (k)
* phylogenetic diversity (branch length)
