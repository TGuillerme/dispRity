# Road to 1.9

 - [ ] do todo below
 - [ ] compile book
 - [ ] run test
 - [ ] compile function index
 - [ ] run CRAN check
 - [ ] add references in .Rd:
   - [ ] reduce.space algorithms
   - [ ] vegan algorithms


## bootstrapping dimensions

 * **New argument** for `boot.matrix`: `what` to specify whether to bootstrap rows (`"rows"` - the default), columns (`"columns"`) or both (`c("rows", "columns")`). Thanks to Gavin Thomas for this suggestion.

 - [ ] implement checks for what
 - [ ] implement checks for dimensions (can now be integer or numeric - number to bootstrap)
 - [ ] implement checks for boot.type (can now also be a named vector like boot.type = c("rows" = "full", "columns" = "single")) (else it's used for both "what" if both are called)
 - [ ] update the dimensions element to be able to accommodate bootstrapped dimensions.
 - [ ] update the dispRity pipeline to call the bootstrapped dimensions.
 - [ ] documentation
 - [ ] test

## RAM helpers

 * General rehaul of the `dispRity` RAM management! All disparity metrics can now have an optional `RAM.help` component that can generate any specific calculation and storage of data for helping the metric. This is then internally used by `dispRity` to pre-calculate and optimise operations that use a lot of RAM or CPU time. For example, you can now use `RAM.help = vegan::vegdist` to pre-calculate all distances in the trait space using `vegan::vegdist`. These pre-calculated distances are then used by the disparity metric avoiding recalculating distances internally. Thanks to Neha Sharma for this suggestion.

 - 1. metrics can now have `RAM.help` arguments that intake a function that will run some pre-calculations. For example, this function can be `vegan::vegdist`.
 - 2. detect the need for RAM help in `get.dispRity.metric.handle`
 - 3. compute heavy calculations at the whole data level in `dispRity` using the `RAM.help` function before the `lapply_loop`
 - 4. store the calculations in `data` similarly as tree as `RAM.helper`
 - 5. run the metrics using a potential `RAM.helper` similarly as tree.
 - [ ] documentation
 - [ ] test
 - [ ] update all the `dispRity` functions that have a `check.dist.matrix` function to use a helper
  neighbours
  span.tree.length
  pairwise.dist
  func.eve
  count.neighbours
  - [ ] do speed test for ego boost

## Generalise pipeline for distance matrices.

 - [ ] add an option `keep.distance = TRUE` to `dispRity` which does:
    - [ ] detect data as a distance matrix or not
    - [ ] if yes + `keep.distance = TRUE` OR if no + `keep.distance = TRUE` + RAM.helper + distance based metric -> apply bootstrap or anything on rows and columns
    - [ ] if no, then old behaviour.
    - [ ] flag new default with warning messages when detecting if yes. "Data is considered as a distance matrix and analysed by keeping the distances (toggle off using `keep.distances = FALSE`").


## Vignettes and manual

 - [ ] make a MCMCglmm related standalone vignette
 - [ ] make a morpho disparity (Claddis) standalone vignette
 - [ ] make a RAM.help section in the manual
 - [ ] update the bootstrap section in the manual with the dimensions
 - [ ] add `count.neigbhours` to the metrics section (*New metric*: `count.neighbours` to count the number of neighbours for each elements within a certain radius (thanks to Rob MacDonald for the suggestion).)

## Minor improvements:
  * Make roundness work for non-VCV matrices (specify the axis function, e.g. variances or quantiles).
  - [ ] TODO 
  * Add vegan::adonis2 ref in adonis.dispRity
  - [ ] TODO

## Bug fixes
 * Check MacOS bugs in the coverage pipeline
 - [ ] TODO

