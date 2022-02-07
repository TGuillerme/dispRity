# TODO list for MCMCglmm branch?

Thanks a lot to Andrew Beckerman, Natalie Cooper and Gavin Thomas for supporting the development of this version.

TODO: update the manual with all these new entries
 - [ ] covar entry
 - [ ] plot arguments options


### NEW FEATURES

 * *New* data function: `select.axes` for selecting and analysing the number of axes required to contain an arbitrary amount of variance.
 * *New* utility function: `randtest.dist` for measuring the distance between the observed statistic and a specific quantile of the simulated statistic (thanks to [Frane Babarovic](https://twitter.com/FBabarovic) for the inspiration).
 * `dispRity` objects can now contain covariance matrices as a `$covar` object. The `covar` part can be directly used for some specific metrics (usually `my_metric.covar`) and are handled by the `dispRity` function (and `plot`, `summary`, etc...) in a specific way. `$covar` contains a list of two elements `VCV` the variance covariance matrix and `loc` the coordinates of the centre of the `VCV` in space (can be left empty). `$covar` matrices are effectively treated as bootstraps.
 * *New function* `covar.plot` for plotting the `covar` content of `dispRity` objects (this is separated from `plot.dispRity` because of the many different options).
 * *New function*: `MCMCglmm.subsets` is a function that allows to convert a `MCMCglmm` object into a `dispRity` object.
 * *New metric*: `projections.between` a between group metric for applying the `projections` metric between the major covariance axis of two matrices.
 * New `dispRity.fast` function for the fastest disparity calculations at the expanses of pretty much everything this package does. This is a really situational function.
 * *New utility functions* for manipulating `MCMCglmm` objects: `MCMCglmm.traits` for extracting the number of traits, `MCMCglmm.levels` for extracting the level names, `MCMCglmm.sample` for sampling posterior IDs and `MCMCglmm.covars` for extracting variance-covariance matrices
 * *New utility functions* for `dispRity` objects with `covar` matrices: `get.covar` to extract the VCV matrices (or a subsample of them); `axes.covar` to extract the major axes of the VCV matrices and `as.covar` to transform `dispRity` metric function to use a covar object.
 * *New utility function* `match.tip.edge` for matching tip labels as factors/characters/numeric to tree edges.
 * Adding `dispRity.covar.projections` function (a wrapper for covariance projections analyses).
 * One new demo datasets: `charadriiformes`, a `data.frame` and a `phylo` object of 359 _Charadriiformes_ species (gulls, plovers and sandpipers) from [Cooney et al 2017](https://www.nature.com/articles/d41586-021-02480-z) along with a `MCMCglmm` model with each clade as a random term.
 * Additional plot arguments `...` in all the `dispRity` plotting functions can now be targeted to a specific plotting element. When multiple elements are plot by default (e.g. lines, legend, points, etc...) it is now possible to pass a specific `...` argument to the specific plotted element using the syntax `<element>.<argument>` (e.g. `points.col = "blue"` will only apply the argument `col = "blue"` to the points).
 
### MINOR IMPROVEMENTS

 * **Changed default arguments** for `projections` and `projections.tree` metrics: the default `"position"` output is now scaled, centred and absolute (see `?projections` for details).
 * Formalised the grouping logic for `custom.subsets` and `select.axes`. This can create some minor user level changes namely: warning messages for empty subsets now correctly mentions "subsets" (rather than subsamples); groups with incorrect elements are now always flagged as errors (rather than just ignored). The changes at the developer level is that the logic is now made smoother and exported in `custom.subsets_fun.R`.
 * Added a `function.index.csv` list (and updater) to help developers find internal functions locations easily.
 * Restricted the type-I error inflation warning message in `test.dispRity` to only occur when using a test of class `"htest"`.
 * Continuous Integration has been moved from Travis-CI to GitHub Actions.
 * `custom.subsets` can now group elements using a `"factor"` vector.
 * Utility functions manuals are now grouped by topic (e.g. utilities related to `MCMCglmm` objects, `dispRity` objects in general, `dispRity` objects with subsets, ect...). It should now be much easier to find these sometimes overlooked functions.
 * Many updates and new entries in the `dispRity` manual, including a section on `covar` and `between.groups` specific analyses.
 * Improving speed for the `test.metric` (using the new official `dispRity.fast` function).
 * Most core functions in the package now have a garbage memory cleaning component. This improves the speed and the memory footprint when handling very large datasets.
 * Disparity results stored in `data$disparity` now don't have dimension names anymore (significantly reducing the size of `disparity` objects). However, you can always retrieve the dimensions names using `get.disparity`.
 * Updated the calculation options for `ellipse.volume`, you can now directly specify one of the following methods: `"pca"` to calculate the eigen values from the ordinated matrix; `"eigen"` to directly do an eigen decomposition of the matrix (new); or `"axes"` to directly measure the axes (new); or directly provide the eigen values.
 * Update internal use of `is(data, c("array", "matrix"))` to `is.array(data)` for R 4.1.2.

### BUG FIXES
 
 * Removed warning in `dispRity` when selecting a specific number of dimensions (old warning artefact).
 * Fixed bug in `plot.dispRity` when using `type = "preview"` on bootstrapped data and for `type = "box"` when the subsets to plot are from different sizes (now plots all the data correctly).
 * Fixed bug when using `chrono.subsets` with `"continuous"` method a `FADLAD` data containing only node values (now correctly taken into account; thanks to [Peng-Wei Li](https://www.researchgate.net/profile/Peng-Wei-Li) for noticing it) and when using `chrono.subsets` with `"gradual.*"` models on empty subsets.
 * `standardGeneric` functions are now correctly interpreted as functions throughout the package.
 * Fixed bug when plotting level 1 disparity metric results without bootstrapped (`observed = TRUE` is now used as the default).
 * Fixed bug when plotting `test.metric` plots with `save.steps` options with more than two types of shifts.
 * Fixed bug with `null.test` which is now correctly managing the number of dimensions inherited from `dispRity` objects (thanks to [Alex Slavenko](https://alexslavenko.weebly.com/) for spotting this one and the two above).
 * Fixed bug when using level 2 dimension metrics on unidimensional data (the metric is now detected as a level 2 correctly; thanks to [Catherine Klein](https://www.researchgate.net/profile/Catherine-Klein) and [Rachel Warnock](https://www.gzn.nat.fau.de/palaeontologie/team/professors/rachel-warnock/) for noticing that one).
 * Update internal use of `is(data, c("array", "matrix"))` to `is.array(data)` for R 4.1.2.

## DEPRECATED
 
 * `matrix.dispRity` and `extract.dispRity` are now deprecated. You should now use `get.matrix` and `get.disparity` respectively instead (with the same options).