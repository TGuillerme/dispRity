TODO list:
Change the structure in:
 - [x] disparity_object
 - [x] make.dispRity
 - [x] clean.data (inc.nodes option)
 - [x] fill.dispRity 
 - [x] chrono.subsets
 - [ ] custom.subsets
 - [ ] dispRity
 - [ ] make.metric
 - [ ] Update metrics
    - [ ] dispRity.metric.R/phylo.projections
    - [ ] dispRity.metric.R/ancestral.dist
 - [ ] print.dispRity
 - [ ] plot.dispRity 
    - [ ] tree networks if multiple trees?
 - [ ] Update dispRity.utilities
 - [x] Add utilities get.phy and add.phy and remove.phy
 - [x] Update the data using make.demo.data
 - [ ] Update the manual
    - [ ] get.tree entry
    - [ ] phy as a part of dispRity entry
 - [ ] Update dtt.dispRity
 - [ ] Update dispRity.wrapper


# NEWS

# New features
 * `dispRity` objects now have a reserved `$phy` component that contain any number of trees attached to the data. This allows any function to use the reserved argument name `phy` to extract directly the relevant tree from the `dispRity` object, for functions like `chrono.subsets` or metrics like `ancestral.dist`! To help manipulate the `phy` component of the `dispRity` object, you can now use the new utility functions `add.phy`, `get.phy` and `remove.phy`.

# Minor improvements
 * Added new option `inc.nodes` to `clean.data` whether to check if the nodes in the tree match the labels in the matrix.








# Manual

## Adding phylogenies to `dispRity` objects (*dispRitree!*)

If phylogeny is going to be an important part of your disparity analyses, you can attach a tree or a list of trees (classes `"phylo"` or `"multiPhylo"`) to your `dispRity` object.
These trees will then be fixed and used where possible throughout your analyses (e.g. when using `chrono.subsets` or disparity metrics based using the argument `phy`).
You can attach trees in different ways, either voluntarily (using @@@) or throughout your analyses through specific functions (@@@).
The tree labels must match the rownames in the data. If the tree has only tip labels, then only these need to be matched, if the tree or the data also has node labels, they must match as well.
