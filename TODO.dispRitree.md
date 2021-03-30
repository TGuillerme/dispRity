TODO list:
Change the structure in:
 - [x] disparity_object
 - [x] make.dispRity
 - [x] clean.data (inc.nodes option)
 - [x] fill.dispRity 
 - [x] chrono.subsets
 - [x] custom.subsets
 - [x] make.metric
 - [x] dispRity
 - [x] Update metrics
    - [x] dispRity.metric.R/projections.tree (phylo.projections)
    - [x] dispRity.metric.R/ancestral.dist
        - [x] add reference.data
    - [x] dispRity.metric.R/edge.length.tree 
    - [x] add example for edge.length.tree + manual entry
 - [x] print.dispRity
 - [x] plot.dispRity 
    - [x] tree networks if multiple trees?
    - [x] fuzzy spaces
 - [x] Update dispRity.utilities
 - [x] Add utilities get.tree and add.tree and remove.tree
 - [x] Update the data using make.demo.data
 - [x] Update the manual
    - [ ] get.tree + utilities entry
    - [x] tree as a part of dispRity entry
 - [ ] Update dtt.dispRity
 - [ ] Update dispRity.wrapper
 - [ ] Update (fix?) `group.dist` to use `get.rotation.matrix`
 - [ ] Update (fix?) `deviations` to use `get.rotation.matrix`
 - [ ] Update dimensions (and `data$call$dimension` to be a vector of dimensions)
        ## Minor
        * The `dimensions` option throughout the package (e.g. in the `dispRity` function) can now also be a vector of dimensions to take into consideration (e.g. `c(1,2,5)`).

# NEWS

# New features
 * `dispRity` objects now have a reserved `$tree` component that contain any number of trees attached to the data. This allows any function to use the reserved argument name `tree` to extract directly the relevant tree from the `dispRity` object, for functions like `chrono.subsets` or metrics like `ancestral.dist`! To help manipulate the `tree` component of the `dispRity` object, you can now use the new utility functions `add.tree`, `get.tree` and `remove.tree`.
 * ... `projections`
 * ... 
 * *New* metric: `projections.tree` that allows to measure elements' projection on axis between elements of a given tree.
 * *New* metric: `edge.length.tree` the edge length from each element given a tree (with the option `to.root = TRUE/FALSE` to measure the edge length from the element to the root of the tree (default = TRUE) or the nearest ancestor (FALSE).


# Minor improvements
 * Added new option `inc.nodes` to `clean.data` whether to check if the nodes in the tree match the labels in the matrix.
 * `make.metric` with the option `silent = TRUE` now outputs a list of info rather than only the level of the metric. You can reproduce the old behaviour using `make.metric(..., silent = TRUE)$type)`.
 * Fixed bug in `plot` using `preview` when the given argument `pch` did not match the number of groups (the different `pch` arguments are now used correctly).
 * Completely revamped the `ancestral.dist` metric. The function is now much faster and much easier to use (due to the new `dispRity` object structure). The options `nodes.coords` has been removed and the option `full` is now changed by `to.root`. If you still really want to use the older version of `ancestral.dist` using `ancestral.dist.deprecated` though.






# Manual

## Adding phylogenies to `dispRity` objects (*dispRitree!*)

If phylogeny is going to be an important part of your disparity analyses, you can attach a tree or a list of trees (classes `"phylo"` or `"multiPhylo"`) to your `dispRity` object.
These trees will then be fixed and used where possible throughout your analyses (e.g. when using `chrono.subsets` or disparity metrics based using the argument `tree`).
You can attach trees in different ways, either voluntarily (using @@@) or throughout your analyses through specific functions (@@@).
The tree labels must match the rownames in the data. If the tree has only tip labels, then only these need to be matched, if the tree or the data also has node labels, they must match as well.

[TODO: add explanation about "reference.data"]
