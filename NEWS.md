dispRity v1.7.10 (2023-02-24)
=========================

### NEW FEATURES
  
 * *New utility function*: `name.subsets` for directly accessing the subsets names of a `dispRity` object (basically doing `names(my_dispRity$subsets)`).
 * *New utility function*: `MCMCglmm.variance` for calculating the variance for specific terms in a `"MCMCglmm"` model.
 <!-- * *New* resampling option for `boot.matrix`: `"null"` to resample each subset with any elements (not only elements within that subset). -->
 <!-- TODO: test + add proba and tree algorithms -->
 <!-- *New metric*: `roundness` to measure how round the elliptical representation of a matrix is. TODO: handle non-VCV input:
 ## The roundness function
roundness <- function(matrix) {
  y <- sort(diag(matrix))
  y <- y/max(y)
  x <- seq(from = 0, to = 1, length.out = length(y))
  sum(diff(x)*zoo::rollmean(y, 2))
}
 -->

### MINOR IMPROVEMENTS

 * **CHANGE IN DEFAULT ARGUMENTS** for the `projections` function. The defaults are now `scale = TRUE`, `centre = FALSE` (previously `TRUE`) and `abs = FALSE` (previously `TRUE`). The default arguments for `dispRity.covar.projections` remain unchanged though (`scale = TRUE`, `centre = TRUE`, `abs = TRUE`).
 * `randtest.dispRity` function's `subsets` argument now can take a list of arguments of subsets to compare is `data` is a `dispRity` object. The call message has also been updated to be much more verbose and clear of what has been tested.
 * optimised internal logic for all the `projections.*` functions for speed.
 * `test.metric` now also adds the slope coefficient if the option `model` is used.
 * the default legends/row names when using `plot`/`summary` on `test.metric` results are now more easy to interpret.
 * `reduce.space` has now 25 times less chances to be stuck on a local optimum.
 * `match.tip.edges` now also works on nodes and on `"multiPhylo"` objects and has a added an option `use.parsimony` (default is `TRUE`) to propagate the matching down the tree (i.e. if two nodes have the same unique ancestor and variable value, the ancestor is consider to have that variable value as well).
 * Improved margins handling in `covar.plot`.
 * Updated test coverage here and there.

### BUG FIXES
 
 * The correct types of changes are now plotted in legend when plotting the results of `test.metric`.
 * `get.disparity` now correctly concatenates one dimensional results into a `"numeric"` (rather than a `"matrix"`).
 * `make.metric` now internally handles `covar` object correctly (i.e. as distance matrices).

dispRity v1.7 (2022-08-08) *MacMacGlimm*
=========================

### NEW FEATURES

 * *New* data function: `select.axes` for selecting and analysing the number of axes required to contain an arbitrary amount of variance.
 * *New* utility function: `randtest.dist` for measuring the distance between the observed statistic and a specific quantile of the simulated statistic (thanks to [Frane Babarovic](https://twitter.com/FBabarovic) for the inspiration).
 * `dispRity` objects can now contain covariance matrices as a `$covar` object. The `covar` part can be directly used for some specific metrics (usually `my_metric.covar`) and are handled by the `dispRity` function (and `plot`, `summary`, etc...) in a specific way. `$covar` contains a list of two elements `VCV` the variance covariance matrix and `loc` the coordinates of the centre of the `VCV` in space (can be left empty). `$covar` matrices are effectively treated as bootstraps.
 * *New function* `covar.plot` for plotting the `covar` content of `dispRity` objects (this is separated from `plot.dispRity` because of the many different options).
 * *New function*: `MCMCglmm.subsets` is a function that allows to convert a `MCMCglmm` object into a `dispRity` object.
 * *New metric*: `projections.between` a between group metric for applying the `projections` metric between the major covariance axis of two matrices.
 * *New metric*: `disalignment`: the (dis)alignment of a group compared to another one (i.e. the rejection from group B's centre on the group A's major axis).
 a between group metric for applying the `projections` metric between the major covariance axis of two matrices.
 * New `dispRity.fast` function for the fastest disparity calculations at the expanses of pretty much everything this package does. This is a really situational function.
 * *New utility functions* for manipulating `MCMCglmm` objects: `MCMCglmm.traits` for extracting the number of traits, `MCMCglmm.levels` for extracting the level names, `MCMCglmm.sample` for sampling posterior IDs and `MCMCglmm.covars` for extracting variance-covariance matrices
 * *New utility functions* for `dispRity` objects with `covar` matrices: `get.covar` to extract the VCV matrices (or a subsample of them); `axes.covar` to extract the major axes of the VCV matrices and `as.covar` to transform `dispRity` metric function to use a covar object.
 * *New utility function* `match.tip.edge` for matching tip labels as factors/characters/integer to tree edges.
 * *New wrapper function* `dispRity.covar.projections` for covariance projections analyses (with its associated S3 sub-class).
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
 * The interval function `check.subsets` now handles the checking of `"dispRity"` objects much faster making most functions relying on it slightly faster (this function is typically not called more than once per function).
 * Updated `adonis.dispRity` to the newest `vegan::adonis2` code (thanks to Jari Oksanen for the notification).
 * Removed dependency to `geiger` for `dtt.dispRity` to avoid package maintenance errors. This leads to no changes at the user level and `geiger::dtt` is still acknowledged in the manual.
 * `tree.age` function's manual now makes it clear it does not estimate tree ages.
 * When using `plot.dispRity(..., type = "preview")`, group's colour attribution and plotting is now made so that the groups larger groups are plotted in the background and the smaller in the foreground.
 * `NA`s are now better handled in internal checking functions. 

### BUG FIXES
 
 * Removed warning in `dispRity` when selecting a specific number of dimensions (old warning artefact).
 * Fixed bug in `plot.dispRity` when using `type = "preview"` on bootstrapped data and for `type = "box"` when the subsets to plot are from different sizes (now plots all the data correctly).
 * Fixed bug when using `chrono.subsets` with `"continuous"` method a `FADLAD` data containing only node values (now correctly taken into account; thanks to Peng-Wei Li for noticing it) and when using `chrono.subsets` with `"gradual.*"` models on empty subsets.
 * `standardGeneric` functions are now correctly interpreted as functions throughout the package.
 * Fixed bug when plotting level 1 disparity metric results without bootstrapped (`observed = TRUE` is now used as the default).
 * Fixed bug when plotting `test.metric` plots with `save.steps` options with more than two types of shifts.
 * Fixed bug with `null.test` which is now correctly managing the number of dimensions inherited from `dispRity` objects (thanks to [Alex Slavenko](https://alexslavenko.weebly.com/) for spotting this one and the two above).
 * Fixed bug when using level 2 dimension metrics on unidimensional data (the metric is now detected as a level 2 correctly; thanks to Catherine Klein and [Rachel Warnock](https://www.gzn.nat.fau.de/palaeontologie/team/professors/rachel-warnock/) for noticing that one).
 * Update internal use of `is(data, c("array", "matrix"))` to `is.array(data)` for R 4.1.2.

### DEPRECATED AND DEFUNCT

 * `matrix.dispRity` and `extract.dispRity` are now deprecated. You should now use respectively `get.matrix` and `get.disparity` instead (the arguments don't change though).

dispRity v1.6.0 (2021-04-16) *dispRitree*
=========================

### NEW FEATURES

 * *New* metric: `projections` that allows to measure elements' projection on an arbitrary axis (or their distance from this axis with `measure = "distance"`).
 * *New* metric: `projections.tree` that allows to measure elements' projection on axis between elements of a given tree.
 * *New* metric: `edge.length.tree` the edge length from each element given a tree (with the option `to.root = TRUE/FALSE` to measure the edge length from the element to the root of the tree (default = TRUE) or the nearest ancestor (FALSE).
 * You can now save the shifts results in `test.metric` with `save.steps` and then visualise them with `plot.dispRity` along side the disparity metric test results.
 * *New* utility function `n.subsets` to directly get the number of subsets in a `dispRity` object.
 * *New* statistical test: `randtest.dispRity` that is a wrapper for `ade4::randtest` applied to `dispRity` objects (not dissimilar from `null.test`).
 * Six more demo datasets have been added to the package! These datasets are the ones used in [Guillerme et al. 2020](https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Shifting+spaces%3A+Which+disparity+or+dissimilarity+measurement+best+summarize+occupancy+in+multidimensional+spaces%3F&btnG=) and published originally in [Beck & Lee 2014](https://royalsocietypublishing.org/doi/full/10.1098/rspb.2014.1278) (that one was originally the only demo dataset in the package), [Wright 2017](https://www.cambridge.org/core/journals/journal-of-paleontology/article/bayesian-estimation-of-fossil-phylogenies-and-the-evolution-of-early-to-middle-paleozoic-crinoids-echinodermata/E37972902541CD0995AAD08A1122BD54), [Marcy et al. 2016](https://link.springer.com/article/10.1186/s12862-016-0782-1), [Hopkins & Pearson 2016](https://pdfs.semanticscholar.org/a3f0/50944d2aefa1df811ea94a3eea630d82c24f.pdf)), [Jones et al. 2015](https://scholar.google.co.uk/scholar?hl=en&as_sdt=0%2C5&q=Dispersal+mode+mediates+the+effect+of+patch+size+and+patch+connectivity+on+metacommunity+diversity&btnG=), [Healy et al. 2019](https://www.nature.com/articles/s41559-019-0938-7). Thanks to all these authors for their open science work!
 * `dispRity` objects now have a reserved `$tree` component that contain any number of trees attached to the data. This allows any function to use the reserved argument name `tree` to extract directly the relevant tree from the `dispRity` object, for functions like `chrono.subsets` or metrics like `ancestral.dist`! To help manipulate the `tree` component of the `dispRity` object, you can now use the new utility functions `add.tree`, `get.tree` and `remove.tree`.

### MINOR IMPROVEMENT

  * Reverted R version requirement from `4.0` back to `3.6` following [Joseph Brown's issue](https://github.com/TGuillerme/dispRity/issues/107) and [fix](https://github.com/TGuillerme/dispRity/pull/108).
  * `reduce.space` `"random"` algorithm now outputs a named logical vector (like the other algorithms!).
  * remove the `"only"` text when printing `dispRity` objects that contains "only" matrices (even though that can be 10k matrices!).
  * added a dedicated behaviour to `summary.dispRity` for `"dispRity"` `"randtest"` objects to output "ready-to-publish" result tables.
  * some error messages have been updated to be slightly more useful.
  * added the `estimation.details` argument to `multi.ace` allowing to also return specific arguments from the ancestral states estimation (thanks to Armin Elsler for the suggestion).
  * Added new option `inc.nodes` to `clean.data` whether to check if the nodes in the tree match the labels in the matrix.
  * `make.metric` with the option `silent = TRUE` now outputs a list of info rather than only the level of the metric. You can reproduce the old behaviour using `make.metric(..., silent = TRUE)$type)`.
  * Fixed bug in `plot` using `preview` when the given argument `pch` did not match the number of groups (the different `pch` arguments are now used correctly).
  * Completely revamped the `ancestral.dist` metric. The function is now much faster and much easier to use (due to the new `dispRity` object structure). The options `nodes.coords` has been removed and the option `full` is now changed by `to.root`. If you still really want to use the older version of `ancestral.dist` using `ancestral.dist.deprecated` though.
  * The `dimensions` option throughout the package (e.g. in the `dispRity` function) can now also be a vector of dimensions to take into consideration (e.g. `c(1,2,5)`).

### BUG FIXES

  * `chrono.subsets` now automatically detects the number of digits to round for the internal time slicing functions (thanks to [Mario Corio](https://mariocoiro.blog/) for finding this one).
  * Fixed bug in `test.metric` plots that now display correctly the "top" and "bottom" changes for the "position" shift.
  * Fixed bug in `test.metric` plots that now display the R^2 values correctly.
  * Fixed bug in `tree.age` when the tree tips/node labels vector is longer than the actual number of tips/nodes in the tree.

### DEPRECATED AND DEFUNCT

  * Removed former version of `ancestral.dist` (see NEW FEATURES above).
  * Removed `node.coordinates` function (no replacement; you must use a package version prior 1.5.10 to use this function).
  * Removed `get.ancestors` function (no replacement; you must use a package version prior 1.5.10 to use this function).

dispRity v1.5.0 (2020-09-25) *between groups*
=========================

### NEW FEATURES

  * *New* function: `multi.ace` for performing fast ancestral character estimations on multiple matrices (based on `castor::asr_mk_model`).
  * *New* function: `reduce.space`, a function to modify trait spaces imported from the [`moms` shiny app](https://github.com/TGuillerme/moms). This function comes with a new reduction algorithm: the "evenness" algorithm for flattening the curve (thanks to [Gavin Thomas](https://github.com/ghthomas) for the suggestion).
  * *New* function: `test.metric` (and associated `plot`, `print` and `summary` functions), to apply the `reduce.space` function on a specific space and metric to test whether a metric is picking up specific changes in trait space.
  * the `dispRity` function can now use `"between.groups"` metrics to calculate disparity between groups rather than within groups. The `make.metric` function is now modified to allow detection of metrics that can be applied between groups.
  * *New* metric: `group.dist`, a dimension level 1 metric for between groups that measures the distance between two groups. By default, this is the minimum distance but the function takes the `probs` argument allowing the distance to be between, says, the 95% CI (`probs = c(0.025, 0.975))`) or between the centroids (`probs = c(0.5)`).
  * *New* metric: `point.dist`, a dimension level 2 metric for between groups that measures the distance between the rows in `matrix` to a point in `matrix2`. That point is the centroid by default but the `point` argument can take any function.

### MINOR IMPROVEMENTS

  * The `dispRity` package now depends on `R (>= 4.0.0)`.
  * Many updates to the `dispRity` manual.
  * Many minor speed improvements across the package
  * Simplified syntax for the internal `plot.dispRity` S3 methods (for a potential `ggpRity`?). These changes should not be apparent at the user level but see the two removed options below:
  * **removed** option in `plot.dispRity`: the `chrono.subsets` option (`TRUE`/`FALSE`) has now been removed. The time-slicing-ness is now automatically detected or can be specified by the user normally through `xlab`.
  * **removed** option in `plot.dispRity`: the `ylim`, `ylab`, `xlab` and `col` options have now been removed. They are now handled through `...` as normal generic `plot(...)` arguments.
  * **removed** option in `plot.dispRity`: the `elements.pch` option has now been removed. The `pch` of the plotted elements can now be passed like other options directly to elements (e.g. `elements = list(pch = 15)`).
  * **removed** option in `plot.dispRity`: the `dimensions`, `matrix`, `nclass` and `coeff` options have now been removed. Any options for dual class plots (`randtest`, `dtt`, `model.test`, `type = "preview"` etc...) are now handled through the generic `specific.args` argument.
  * *New* option in `dtt.dispRity`: `scale.time` allowing to scale the time axis (like in `geiger::dtt`) or not.
  * when plotting `chrono.subsets` `dispRity` objects, the x label ticks are now rounded if possible (for nicer looking plots!).
  * when using automatic `chrono.subsets` time slices, the name of the time slices (their age) is now rounded for aesthetics.


### BUG FIXES

  * Updated all calls to the `Claddis` package to match version 0.6 (thanks to [Graeme Lloyd](http://graemetlloyd.com/) for fixing [this one](https://github.com/TGuillerme/dispRity/pull/104)!)
  * Updated `Claddis.ordination` function to be compatible with the new `ape` version `5.4` (thanks to [Emmanuel Paradis](https://github.com/emmanuelparadis) for the pointing that out).
  * Fixed a bug in `chrono.subsets` where ladder trees with no tip branch lengths an `method = "continuous"` option would get stuck in an infinite loop.
  * Fixed a bug in `chrono.subsets` where the `"*.split"` methods would bug if the last slice is through a single edge.
  * Fixed a bug in `dispRity` where some 1D matrices could loose their class for certain metrics.

dispRity v1.4.0 (2020-05-05) *getting faster*
=========================

### NEW FEATURES

 * *New* metric: `angles`, a dimension level 2 metric that measures the angle of the main axis of each dimension in a matrix (in slopes, angles or degrees).
 * *New* metric: `deviations`, a dimension level 2 metric that measures the deviation of each element from a hyperplane.
 * Completely rewritten `char.diff` function. It now uses a way faster bitwise comparison architecture and comes with different distance methods as well as modular optional arguments on how to treat various special tokens (`"?"`, `"-"`, `"&"`, etc.). This also allows many more distance methods (now including `"hamming"`, `"manhattan"`, `"comparable"`, `"euclidean"`, `"maximum"` and `"mord"`).
 * all `dispRity` functions can now intake a single `"matrix"` or a `"list"` of matrices with the same row names and dimensions. The disparity is then calculated directly on all the matrices and summarised as before through `summary.dispRity`. This option can be used to add uncertainty to disparity calculations. For example in `chrono.subsets` you can now provide a list of trees and a list of associated ancestral state estimates; or for `custom.subsets` you can provide a list of matrices with different values representing different estimations of the traits.

### BUG FIXES

  * update `reduce.matrix` to work with `vegan::vegdist` version 2.5-6 (thanks to [Jari Oksanen for the fix](https://github.com/TGuillerme/dispRity/pull/85)).
  * updated class evaluations throughout the package for `R` version `4.0.0`: `class(.) == *` is now `is(., *)`.
  * updated `...` argument bug PR#16223.
  * In `make.metric` the argument `...` is now ignored if any `names(...)` is `"tree"` or `"phy"`.
  * fixed bug in `neighbours` and `span.tree.length` when feeding "distance" like metrics (thanks to Ashley Reaney for finding that one).

### MINOR IMPROVEMENTS

  * greatly improved speed of `chrono.subsets` with `method = "continuous` (now > 1000 times faster!).
  * minor warning message fix for `plot.dispRity` with time slices.
  * removed `paleotree` package dependency (though the links to this excellent package are still there!).
  * increased `R` version requirement to `3.5`.
  * `...` in `summary.dispRity` are now directly passed to `cent.tend` (e.g. `na.rm = TRUE`).
  * added some time improvements in several phylo functions based on the `castor` package.
  * updated all the package demo data to fit the new `dispRity` object architecture (see above). Note that this might effect the exact results of calculations using these demo datasets.
  * you can now specify the dimensions of the matrix to make a disparity metric in `make.metric` through the `data.dim` option. 
  * metrics passed in `dispRity` are now tested using the input data dimensions.
  * `chrono.subsets` with multiple trees now stretches the root edges length to match the oldest tree.


dispRity v1.3 (2019-08-15) *many metrics*
=========================

### NEW FEATURES

 * *New* metric: `displacements`, a dimension level 2 metric that measures the position of elements in space (i.e. their distance from the centre relative to their distance to their centroid).
 * *New* metric: `neighbours`, a dimension level 2 metric that measures the distance from an element to its neighbour (e.g. the nearest neighbour, the furthest, the median, etc.).
 * *New* metric: `quantiles`, a dimension level 2 metric that measures the *n*th quantile range per axis (a good alternative to the `ranges` function!).
 * *New* metric: `func.eve`, a dimension level 1 metric that measures the functional evenness (i.e. the spread along the minimum spanning tree; from Villéger et al. 2008).
 * *New* metric: `func.div`, a dimension level 1 metric that measures the functional divergence (i.e. the ratio of deviation from the centroid; from Villéger et al. 2008).
 * *Updated* metric: `span.tree.length` now outputs the length of each edges (c.f. the sum of the length) and becomes a level 2 metric.
 * The `chrono.subsets` can now take `multiPhylo` objects for slicing through multiple trees at once!
 * *New* utility function: `reduce.matrix` for optimising data overlap in a matrix with missing data.
 * *New* utility function: `slide.nodes` for sliding specific nodes on a tree.
 * *New* utility function: `remove.zero.brlen` for stochastically removing zero branch lengths on a tree (using the `slide.nodes` function).
 * **New argument** in `plot.dispRity`: the `type` argument can now be `"preview"` to have a glimpse at two of the dimensions of the trait-space.
 * The `Claddis.ordination` can now directly take a matrix's path as input (leaving the function to read and transform the matrix into `Claddis` format. The function can thus now also be used to convert matrices into `Claddis` format.
 * Added a "Other functionalities" section to the manual describing miscellaneous functions.

### MINOR IMPROVEMENTS

 * `centroids` and `ancestral.dist` functions can now take the `method` option for `"euclidean"` or `"manhattan"` distances.
 * All functions methods selection have now been sped up using `switch`.
 * Error messages in `dispRity` are more verbose when input the wrong metric(s).
 * `scree` option in `space.maker` does not require to sum up to one anymore.
 * `cor.matrix` option in `space.maker` does not require to have a valid Choleski decomposition (an approximation is used instead).
 * Updated all tests and functions to be compatible with R 3.6.
 * Fixed bug in `clean.data` that did not output dropped tips correctly when applied on `multiPhylo` objects.
 * Improved error messages in `chrono.subsets` for funky time slices/bins (e.g. with negative values).
 * Speed improvements for the `time.slice` function.
 * Better internal handling of distance matrices for the disparity metrics.
 * Most functions handles `NA` as `na.rm` or `na.omit`.

dispRity v1.2.3 (2019-03-12)
=========================

### NEW FEATURES

 * `dispRity` objects now contain a metric argument (if a metric was applied). This argument can now be recycled by the appropriate functions (e.g. in `null.test`).

### MINOR IMPROVEMENTS

  * `plot.dispRity` argument `observed` can now take a list of arguments to be passed `points()`.
  * `boot.matrix` now makes a error warning message when bootstrapping distance matrices (as suggested by [Dave Bapst](https://github.com/dwbapst/)!).
  * `geomorph.ordination` can now be used to simply create coordinates matrices (no ordination!) with `ordinate = FALSE` argument.
  * better internal handling of error messages.
  * `faster` eigen value estimations in `ellipse.volume` when the argument is left missing.
  * removed internal handling of the `Claddis.ordination` function. This function now uses the brand new version of the `Claddis` package on CRAN (0.3).
  
### BUG FIXES

  * `plot.dispRity` with option `"box"` now correctly display plot ranges when disparity is an observed distribution.
  * `test.dispRity` handles errors messages more efficiently when disparity is an observed distribution.
  * `summary.dispRity` handles non-bootstrapped distributions display properly.
  * `geomorph.ordination` now converts `"character"` vectors into `"factors"`.
  * `adonis.dispRity` now properly handles complex formulas (with arithmetic signs).
  * `...`  are now properly handled by internal metric testing functions for more accurate error messages.
  * `char.diff` names are now properly protected in the `C` implementation to comply with new `rcheck` requirements.

dispRity v1.2 (2018-09-19) *model tests*
=========================

### NEW FEATURES

  * *New* functions: `model.test`, `model.test.sim` and `model.test.wrapper` for fitting models of disparity evolution through time (with associated manuals, vignettes and `S3` methods! Thanks to [Mark Puttick](https://github.com/PuttickMacroevolution)).
  * **New argument** in `boot.matrix`: `prob` for passing probabilities of sampling for specific elements.
  * S3 `print` method for objects of class `"dtt"` and `"dispRity"` (from `dtt.dispRity`).

### MINOR IMPROVEMENTS

  * tydiversed most of the error messages.
  * `dtt.dispRity` now allows to specify the alternative hypothesis (if `nsim > 0`).
  * `ellipse.volume` can now take an explicit eigen value vector (the eigen values are still automatically estimated correctly for PCO and MDS).
  * Improved metric checking messages from `make.metric` when dealing with optional arguments.
  * Removed cascade of warnings triggered by `plot.dispRity.dtt`.

### BUG FIXES

  * Corrected `char.diff` to properly reflect the probability of different splits between characters (thanks to [Abigail Pastore](https://github.com/aipastore)).


dispRity v1.1 (2018-03-20) *got CRAN*
=========================
  
### NEW FEATURES

  * CRAN release 1 with the additional `Claddis.ordination` function.

### MINOR IMPROVEMENTS

  * Added default Cailliez correction to `Claddis.ordination` function (with `add = TRUE`).
  * Improved test coverage.

dispRity v1.0.3 (2018-03-20) *got CRAN*
=========================
  
### NEW FEATURES

 * First CRAN release

### MINOR IMPROVEMENTS

  * Registered `C` symbols properly.
  * Overall improvement for the `S3` methods.
  * **Changed name**: `scale.dispRity` is now `rescale.dispRity`.
  * **Changed name**: `merge.subsets` is now `combine.subsets`.
  * **Changed name**: `time.subsets` is now `chrono.subsets` - `time.subsets` can still be called as an alias for the same function.

### BUG FIXES

  * Minor bug fixes and typos in various error messages and in the manual.

### DEPRECATED AND DEFUNCT

  * Removed `Claddis.ordination` function to comply with the CRAN requirement (this function is still live in the [GitHub version](https://github.com/TGuillerme/dispRity) 1.1).

dispRity v0.5 (2017-12-20) *covered with tests*
=========================

### NEW FEATURES

  * `custom.subset` can now automatically create clade-based groups if a `phylo` object is passed to `group`.
  * *New* utility function: `extinction.subsets`, to get the list to be passed to `test.dispRity` for testing the effect of extinction.
  * *New* test function: `dtt.dispRity`, a wrapper for [`geiger::dtt`](https://github.com/mwpennell/geiger-v2). This version is slower that `geiger::dtt` but allows any univariate disparity metric!
  * *New* test function: `adonis.dispRity`, a wrapper for [`vegan::adonis`](https://github.com/vegandevs/vegan).
  * *New* utility function: `crown.stem` for separating a tree into crown and stem groups.
  * *New* disparity metric: `span.tree.length` the length of the minimum spanning tree.
  * *New* disparity metric: `pairwise.dist`: the element's pairwise distances.
  * *New* disparity metric: `radius`: the radius of each dimensions.
  * *New* disparity metric: `n.ball.volume`: the *n*-dimensional sphere or ellipsoid volume.
  * **New argument** in `time.subsets`, `model = "equal.split"` and `model = "gradual.split"` that retain the probability of being either the descendant or the ancestor. This probability is passed to `boot.matrix` .
    
### MINOR IMPROVEMENTS

  * Changed calls to `stats::dist` to `vegan::vegdist` to allow more distances to be passed through `methods` arguments.
  * `slice.tree` can now slice through a single edge.
  * Various minor speed improvements.

### BUG FIXES

  * Correct behaviour in `tree.age` to estimate ages for trees with fossils only.

### DEPRECATED AND DEFUNCT

  * **Change name** throughout the package, `subsample` is now replaced by `subset` (e.g. `time.subsamples` is now renamed `time.subsets`, `data$subsamples` is now `data$subsets`, etc...).
  * **Changed argument** in `time.subsets`, `model = "gradual"` is now replaced by `model = "proximity"` and `model = "punctuated"` is now replaced by `model = "random"`.
  
  
dispRity v0.4.1 (2017-11-13)
=========================

### NEW FEATURES

  * *New* disparity metric: `ancestral.distance` to get the distance from taxa/nodes to their ancestors.
  * *New* function: `random.circle` for generating random circle coordinates (see example in `space.maker` for creating doughnut spaces!).
  * *New* function: `get.bin.ages` for getting the geological timescale of a tree (based on `geoscale`).

### MINOR IMPROVEMENTS

  * Added a `t0` argument to `time.subsamples` allowing to set the start age of the first subsample.
  * Allowing subsamples to contain less than three elements (up to 0!).

### BUG FIXES

  * Fixed fuzzy match issues in `slice.tree`.


dispRity v0.4 (2017-08-21) *user friendly*
=========================

### NEW FEATURES

  * Entirely rewritten manual (in GitBook)!
  * *New* function: `Claddis.ordination` and `geomorph.ordination` for automatically ordinating data from `Claddis` and `geomorph` packages!
  * *New* function: `char.diff` for calculating character differences and associated plot function (`plot.char.diff`)
  * *New* utility function: `merge.subsamples` for... merging subsamples.
  * *New* utility function: `size.subsamples` for getting the size of subsamples in a disparity object.
  * *New* wrapping functions: `dispRity.through.time` and `dispRity.per.group` now runs easy default disparity analysis.

### MINOR IMPROVEMENTS

  * Input ordinated matrices do not need to be of maximum size `n*(n-1)`. Bigger matrices now only trigger a warning.
  * Added `dimensions` optional argument to `dispRity` to overwrite the number of dimensions generated by `boot.matrix`.
  * `variances`, `ranges` and `centroids` are now simplified for speed. The optional arguments for data cleaning are now passed to `make.metric`.
  * `space.maker` now allows to approximate the dimensions variance distribution with the `scree` option.

### DEPRECATED AND DEFUNCT

  * **Removed** `hyper.volume` metric for dependencies reasons,
  * **Removed** `parallel` option from `boot.matrix` (the new architecture is already super fast: <2sec for 5k taxa and 10k bootstraps!).
  * **Changed name**: `series` as a part of `dispRity` objects is now changed to `subsamples` throughout the whole package.
  * **Changed name**: `time.series` is now renamed `time.subsamples`, if dates are provided and method is `discrete`, this function doesn't need a phylogeny any more.
  * **Changed name**: `get.subsamples.dispRity` is now renamed `get.subsamples`.
  * **Modified function**: `cust.series` is now renamed `custom.subsamples` (to avoid confusion with `custard.subsamples`!). Its `factor` argument as been changed to `groups` and can now take a simple list.


dispRity v0.3 (2017-01-25) *dispRity lite*
=========================

### NEW FEATURES

  * Complete change of the `dispRity` object architecture (see more [here](https://github.com/TGuillerme/dispRity/blob/master/disparity_object.md)).

### MINOR IMPROVEMENTS

  * `sim.morpho` can now use `model = "mixed"` for using both `HKY-binary` and `Mk` in characters simulation.
  * `dispRity` object utilities are now all grouped under the `?dispRity.utilities` manual with appropriate S3 methods.
  
### DEPRECATED AND DEFUNCT

  * **Removed** `rm.last.axis` argument in `boot.matrix`. It is now replaced by `dimensions`.
  * **Changed argument** in `plot.dispRity`, `type = "lines"` is now replaced by `type = "line"`.

dispRity v0.2.1 (2016-11-07) *making stuff up*
=========================

### NEW FEATURES

  * *New* utility function: `merge.time.series` for cleaning or merging time series,.
  * *New* vignette: `dispRity-simulate_data` on how to simulate morphological characters in `dispRity`.
  * *New* function: `sim.morpho` generates morphological matrices.
  * *New* function: `check.morpho` for checking how "realistic" the simulate morphological matrices are.
  * *New* utility functions: `get.contrast.matrix` and `apply.inapplicable` functions for morphological matrices.

### MINOR IMPROVEMENTS

  * Minor updates for the graphical functions.

dispRity v0.2 (2016-06-15)
=========================

### NEW FEATURES

  * *New* utility functions: `pair.plot`, `scale.dispRity` and `sort.dispRity`.
  * *New* function: `space.maker` for creating some multidimensional spaces!
  * *New* disparity metrics: `convhull.surface`, `convhull.volume` and `hyper.volume`.
  * *New* disparity test `null.test`.
  * *New* `plot.dispRity` arguments: `density` for controlling the polygons density and `add` for adding plots.

### MINOR IMPROVEMENTS

  * Many updates to the functions manual and vignettes.
  * Some algorithm are now greatly improved for speed.
  * Disparity can now be calculated as a distribution (i.e. `dispRity`, `test.dispRity`, `plot.dispRity`, and `summary.dispRity` can now intake one or more distribution rather than just one or more single values of disparity; whether the data is bootstrapped or not).
  * `dispRity` can now intake `dispRity` objects with level 2 disparity metrics.
  * `boot.matrix` and `dispRity` can now run in parallel.
  * `centroids` disparity metric can now use a `centroid` argument for fixing the centroid point value.
  * `variances` and `ranges` disparity metrics can now intake a `k.root` argument for scaling the results.

### BUG FIXES

  * Minor functions corrections for specific optional arguments combinations.

### DEPRECATED AND DEFUNCT

  * **Removed** `type_discrete` argument in `plot.dispRity` and `type` argument can now be:
    * `continuous` disparity curves.
    * `box` for real boxplots.
    * `lines` for the distribution vertical lines.
    * `polygon` for the distribution boxes.
    
dispRity v0.1.2 (2015-12-01)
=========================

### NEW FEATURES

  * *New* function: `get.dispRity` for subsampling dispRity objects.
  * *New* function: `extract.dispRity` for extracting disparity results.
  * *New* function: `test.dispRity` for applying tests to `dispRity` objects.
  * *New* function: `make.metric` for helping creating your very own disparity metric.
  * *New* metric: `hyper.volume` for measuring the morphospace hyper-ellipsoid volume.

### MINOR IMPROVEMENTS

  * `metric` argument from `dispRity` can now intake up two three functions (see `dispRity.metric` and `make.metric`).
  * Many improved functions manuals and examples!
  * Improved vignettes:
    * *dispRity palaeo demo* a quick demo aimed more for palaeobiologist.
    * *dispRity ecology demo* a quick demo aimed more for ecologists.
    * *dispRity manual* for people that want to know the package in details.
    * *dispRity metrics* for explaining how the disparity metric implementation works.
    
dispRity v0.1.1 (2015-10-08)
=========================

### MINOR IMPROVEMENTS

  * `plot.dispRity` options improved (rarefaction + default).
  * `cust.series` can now intake multiple factors columns.
  * Added example for ecological data.
  * `boot.matrix`, `dispRity`, `summary` and `plot` now also include observed values.
  * `plot` now has an `observed` option to plot the observed disparity.

### BUG FIXES

  * Fixed many error/warning messages.
  
### DEPRECATED AND DEFUNCT

  * Changed `taxa` to `elements`.
  * `plot` option `diversity` has been renamed `elements`.


dispRity v0.1 (2015-10-01)
=========================
  * First release!
