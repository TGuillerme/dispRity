# TODO list for MCMCglmm branch?

TODO: (no NEWS export)
- [x] set up testing data for MCMCglmm
- [x] updated `disparity_object.md` structure

TODO: transfer from `beer`:
 - [ ] change `data$MCMCglmm$covars` -> `data$covar`
 - [ ] change `data$MCMCglmm$covars[[1]]$Sol` -> `data$covar[[1]]$centre`
 - [ ] make a list of new functions here and check sanitizing (SEE BELOW)
       * [ ] `MCMCglmm.traits`
       * [ ] ...
 - [ ] make a list of warnings/stops and `dispRity`ze them
       * [ ] ...







 * `dispRity` objects can now contain covariance matrices as a `$covar` object. The `covar` part can be directly used for some specific metrics (usually `my_metric.covar`) and are handled by the `dispRity` function (and plot, summary, etc...) in a specific way.`$covar` contains a list of two elements `VCV` the variance covariance matrix and `centre` the coordinates of the centre of the `VCV` in space (can be left empty). `$covar` matrices are effectively treated as bootstraps.
 - [ ] implemented
 - [ ] tested
 - [ ] example

<!--  * `make.dispRity` now has a `covar` argument.
 - [ ] implemented
 - [ ] tested
 - [ ] example

 * `custom.subsets` now has a `covar` argument.
 - [ ] implemented
 - [ ] tested
 - [ ] example -->

 * *New utility functions* for `MCMCglmm` objects
        * `MCMCglmm.traits`
        * `MCMCglmm.levels(MCMglmm, ...)`
        * `MCMCglmm.sample(MCMglmm, n, ...)`
        * `MCMCglmm.covars(MCMglmm, n, sample, ...)` #TODO: rename into `MCMCglmm.covar`
 - [x] implemented
 - [x] tested
 - [x] example
 - [ ] optimise speed?

 * *New utility functions* for `dispRity` objects with `covar` matrices
        * `get.covar` to extract the VCV matrices (or a subsample of them)
        * `axes.covar` to extract the major axes of the VCV matrices
 - [x] implemented
 - [x] tested
 - [x] example
        * `as.covar` to transform `dispRity` metric function to use a covar object.
 - [ ] implemented
 - [ ] tested
 - [ ] example



 * *New function* `sauron.plot` for plotting the `covar` content of `dispRity` objects (this is separated from `plot.dispRity` because of the many different options).
 - [ ] implemented
 - [ ] tested
 - [ ] example

 * *New function* `MCMCglmm.subsets` is a function that allows to convert a `MCMCglmm` object into a `dispRity` object.
 - [x] implemented
 - [x] tested
 - [x] example
 - [ ] optimise speed?

 * *New metric* `projections.covar` a between group metric for applying the `projections` metric on `covar` matrices.
 - [ ] implemented
 - [ ] tested
 - [ ] example

 * New `dispRity.fast` function for the fastest disparity calculations at the expanses of pretty much everything this package does. This is a really situational function.
 - [ ] implemented
 - [ ] tested
 - [ ] example

 * One new demo datasets: `charadriiformes`, a `data.frame` and a `phylo` object of 50 random _charadriiformes_ species (gulls, plovers and sandpipers) from [Cooney et al 2017](https://www.nature.com/articles/d41586-021-02480-z) along with a `MCMCglmm` model with each clade as a random term.

## Minor

 * Utility functions manuals are now grouped by topic (e.g. utilities related to `MCMCglmm` objects, `dispRity` objects in general, `dispRity` objects with subsets, ect...). It should now be much easier to find these sometimes overlooked functions.
 - [x] implemented
 - [x] tested


 * Add a `between.groups` tutorial??