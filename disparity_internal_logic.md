# `dispRity` function internal logic explained

This is a note for understanding the logic at the base of the package within the `dispRity` function (i.e. to calculate disparity metrics).

The disparity calculations are handled by two main functions `mapply.wrapper` or `lapply.wrapper` depending if the metric is applied between groups or not (respectively). Here we are going to focus on `lapply.wrapper` but `mapply.wrapper` uses the same logic but intakes two lists instead of one.

## The inputs

The `lapply.wrapper` function intakes the following mandatory arguments:

 * `lapply_loop` (`"list"`) that contains the row names to analyse for each subsest (each element of the list is divided into different lists containing the observed rows as a n x 1 matrix and the bootstrapped rows as a n x bootstraps matrix as well as the rarefaction levels (see [`disparity_object.md`](https://github.com/TGuillerme/dispRity/blob/master/disparity_object.md) for details). If disparity was already previously calculated, `lapply_loop` contains disparity values (`"matrix"` or `"array"`) rather than row names.
 * `metrics_list` (`"list"` of length 3) the list of the three levels of metrics to apply (some levels can be NULL).
 * `data`  (`"dispRity"`) the data that must contain at least the matrix or the disparity data and the number of dimensions (and also a tree if `metric_is_tree`).
 * `matrix_decomposition` (`"logical"` of length 1) whether to decompose the matrix (see later)
 * `verbose` (`"logical"` of length 1) whether to be verbose
 * `metric_is_tree` (`"logical"` of length 3) whether each metric needs a tree
 * `...` any arguments to be handled by the functions in `metrics_list`

 * `metric_is_between.groups` which is a deprecated placeholder for between groups analyses
TODO: remove that!

## The pipeline

These arguments are passed to `lapply.wrapper` (or `mapply.wrapper`) which first toggles the functions to be verbose or not and then handles in turn the following (in a nested way):

### 1 - Passing one subset (e.g. `lapply_loop[[1]]`) to `disparity.bootstraps`

The `disparity.bootstraps` function handles subsets one by one, (e.g. `lapply_loop[[1]][[1]]`, etc...) and calculates disparity on them using the different metrics from `metrics_list`.

This function is composed of two elements: firstly decomposing the matrix if necessary (see point 2 below) and secondly applying each metric depending on there level. The application of each metric is done simply by using an apply loop like `apply(data, margin, metric)` where `data` is already calculated disparity data (either from previous calculations or from decomposing the matrix), `margin` is detected automatically and `metric` comes from `metrics_list` according to the correct level.

### 2 - Wrapping the matrix decomposition for one subset with `decompose.matrix.wrapper`

This part transforms the raw data from `"dispRity"` (i.e. the row IDs from the subset, the requested number of dimensions and the original matrix) into the first requested disparity metric (with the appropriate requested level, e.g. if the metric contains `var`, the matrix decomposition transforms the `"dispRity"` data into a var/covar `"matrix"`). The metric to use to decompose the matrix (the one with the highest level) is detected with the `get.first.metric` function. Then both the `"dispRity"` data and the first metric are passed to `decompose.matrix.wrapper`.

This function basically feeds the number of subsets (i.e. either elements (n x 1) or the bootstraps/rarefaction (n x bootstraps) - see [`disparity_object.md`](https://github.com/TGuillerme/dispRity/blob/master/disparity_object.md) for details) to the `decompose.matrix` function along with the first metric.
The wrapper does also a bit of data/format wrangling I'm not going to detail here (and can hopefully be understood reading the code).

### 3 - Decomposing the matrix with `decompose.matrix`

This function intakes:

 * `one_subsets_bootstraps` that is one row from one element of the subset list (e.g. `lapply_loop[[1]][[1]][, 1]`). Effectively, these are the row IDs to be considered when calculating the disparity metric.
 * `fun` the first disparity metric
 * `data` the `"dispRity"` object
 * `nrow` an option for between groups calculation. Is `NULL` when the decomposition is for a normal metric (but is an indicator of the pairs of row to consider if the metric is `between.groups = TRUE`).

And then applies the `decompose` function to each matrix in `data$matrix`. Which simply applies the metric to the matrix in the following format (i.e. `fun(matrix[rows, columns])`).