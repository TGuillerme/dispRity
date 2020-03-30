# mulTree shift:

Changing the `dispRity` structure to allow list of matrices

## NEWS:

* All `dispRity` functions can now intake a single `"matrix"` or a `"list"` of matrices with the same row names and dimensions. The disparity is then calculated directly on all the matrices and summarised as before through `summary.dispRity`. This option can be used to add uncertainty to disparity calculations. For example in `chrono.subsets` you can now provide a list of trees and a list of associated ancestral state estimates; or for `custom.subsets` you can provide a list of matrices with different values representing different estimations of the traits.

# Individual function changes

## tests
 
 - [x] update all tests

## `make.dispRity`

 - [x] make a list of one matrix.

## `check.dispRity.data`

 - [x] checks data to be matrix or list.
 - [x] if list check dimnames equal
 - [x] if no.dimnames add integers
 - [x] sort dimnames
 - [x] return list of data

## `fill.dispRity`

 - [x] make a list of N matrices (using `check.dispRity.data`). 

## `dispRity`

 - [x] use check.dispRity
 - [x] apply the disparity calculation to all the matrices
 - [x] return disparity values as observed (length = length(matrices)) and normal bootstraps
 - [x] test on all disparity tests
 - [x] update example data
 - [ ] works with subsets with multiple trees and multiple matrices

## `dispRity.utilities`

 - [x] `matrix.dispRity`  works on list rather than matrices

## `print.dispRity`

 - [x] now also print the number of matrices

## `plot.dispRity` 

 - [x] `type = "preview"` has a matrix selector

## `boot.matrix`

 - [x] bootstraps all the matrices option (default): picks the row numbers for the matrices regardless of the matrix ID (number). Similar option as before
 - [ ] works with subsets with multiple trees and multiple matrices

## `custom.subsets`

 - [x] creates the subsets for each matrices (matches the dimensions names).

## `chrono.subsets`

 - [x] `chrono.subsets` works with list of matrices.

## `null.test`

 - [x] pass the elements for simulating the space from the list of matrices rather than a single matrix.

## `dispRity` data

 - [x] update `BeckLee_XXX` data
 - [x] update `disparie ty` data
 - [x] update `BeckLee_disparity` data

## `dispRity` manual

 - [ ] update guts part
 - [ ] update examples
 - [ ] add `extinction.subsets` somewhere?