TODO list for v1.6 **dispRitree**

### NEW FEATURES

 * [x] *New* metric: `phylo.projections` that allows to measure elements' projection on axis between elements of a given tree. @master
    - [x]: add test
    - [x]: add example
    - [x]: add in manual
    - [ ]: make work in `dispRity`

 * [ ] `dispRity` can now directly link one or more trees (`dispRity$phy`) to their matrices. @dispRitree
 * [ ] `phy` is now a reserved term for trees linked to a `dispRity` object and is automatically handled in `dispRity.metric` function. In practice, any disparity that has an argument `phy` will try to use the `dispRity$phy` tree associated with the matrix on which it's applying the disparity metric. Thus every disparity metric with an argument `tree` now has this argument changed to `phy`, namely: @dispRitree
   - [ ] `ancestral.dist` @dispRitree
   - [ ] `phylo.projection` @dispRitree

Change `$phy` in:

 *




 * [ ] `make.dispRity` now automatically tries to "*compactify*" matrices if possible to create a much lighter footprint in R's memory (using the argument `compact = TRUE`). The "*compactification*" works by factoring rows in list of matrices that are invariant (e.g. when the tips are constant and the nodes are variable). @compactify
 * [x] new statistical test: `randtest.dispRity` that is a wrapper for `ade4::randtest` applied to `dispRity` objects. @master
    - [x] associated `print.dispRity.randtest`
    - [x] associated `summary.dispRity.randtest`
    - [x] associated `plot.dispRity.randtest`

# Update manual
 - [ ] `phylo.projections`
 - [ ] update `disparity_object.md` with phylo structure
 - [ ] add entry to the manual about phylo structure
 - [ ] update `disparity_object.md` with compact structure
 - [ ] add entry to the manual about compact structure