TODO list for v1.6 **dispRitree**

### NEW FEATURES

 * [ ] `make.dispRity` now automatically tries to "*compactify*" matrices if possible to create a much lighter footprint in R's memory (using the argument `compact = TRUE`). The "*compactification*" works by factoring rows in list of matrices that are invariant (e.g. when the tips are constant and the nodes are variable). @compactify


# Update manual
 - [ ] `phylo.projections`
 - [ ] update `disparity_object.md` with phylo structure
 - [ ] add entry to the manual about phylo structure
 - [ ] update `disparity_object.md` with compact structure
 - [ ] add entry to the manual about compact structure