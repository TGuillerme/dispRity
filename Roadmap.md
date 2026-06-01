# Road map for the Deltatronic update

This is gonna be integrated in the next release 1.10 (including the `BAT` compatibility + other stuff).

## Functions wish list for Deltratronic

# 1 - user level function `chrono.test`

One function called `chrono.test` that contains the series:

#'@param method, "character" that can be any of the following:
 * `"itsa"`
 * `"citsa"`
 * `"area"`
 * `"average"` (`t.test`) or any function that outputs a `"h.test"` object (e.g. `"t.test"`, or `aov`) (average)

#'@param changepoint, a vector of at least one "numeric", "integer" or "character" `"detect"`
 
#'@param time.window, a vector of 2 "numeric", "integer" that designate the time window around changepoint. OR a single "numeric", "integer" for the number of datapoints before AND after (e.g. time.window = 3, will use three before and three after), OR a "numeric" <0.5, that will be a percentage of the data to integrate before and after (time.window = 0.5 will include all the data)

#'@param ... is a named list of specific arguments for each method

```r
chrono.test(disparity, method, changepoint, time.window, ...)
```

### TODO:

 - [ ] write the doc (with these arguments above)
 - [ ] implement the check.* for these four arguments
 - [ ] do the sanity unit testing (e.g. if `chrono.test(disparity, method = "kista", changepoint, time.window, ...)` error should be: "Wrong method type, the ones allowed are...")

# 2 - under the hood functions

## `check.time` function for sanitizing both `changepoint` and `time.window` args 
  - [ ] unit test
  - [ ] implement

## `check.dispRity` function to sanitize data input:
   *  has both a tree and matrix (check the tree and matrix match)
   *  check if it is multi or not (i.e., sometimes will be using sample ace data, so n(matrix) > 1 )
   *  check if it has subsets defined already. Needs to be from chrono.subsets OR custom.subsets with mega conditions TODO:for later version
   *  check if it has disparity values + the disparity level
  - [ ] unit test
  - [ ] implement

## `make.deltatronic`
   * at base level, is data.frame of: time | disparity | intervention
   * has conditional calls depending on `method = ...`, i.e. `if(method == "citsa") {delta_df$real_vs_control <- c(rep(1, nrow(real)), rep(0, nrow(cont)))} 
  - [ ] unit test
  - [ ] implement

## `set.time.window`
  through the input `dispRity` object + the input options.
  - [ ] unit test
  - [ ] implement

## `set.changepoint`
  generating a list of changepoints to go through (each in a absolute time format (e.g. 66)).
  If only one changepoint, list contains only one element, else more. If `changepoint = "detect"` by default it creates a list using the input time slices, else you can use `changepoint = list(method = "detect", resolution = 2)` which will create a list of every 2 mya (within the time window).
  - [ ] unit test
  - [ ] implement

## `itsa.method`  
   * `run.itsa.model` runs `lm` function on the `make.deltatronic` output
      * Add option to have not only `lm` (`... <- list("itsa.model" = lm)`)
  - [ ] unit test
  - [ ] implement

## `citsa.method` 
  
1 - Generate the following ... options as default (or extract from user e.g. `... <- list(nsim = 20, covar.estimate = TRUE)`)    
  * nsim = number of simulations
  * covar.estimate = whether to estimate the covariance structure between traits based on the pre-change point? (TRUE; default), or FALSE if you assume that your dimensions are orthogonal.
  * Add option to have not only `lm` (`... <- list("itsa.model" = lm)`)
  * if paint = TRUE calls `paint.branches`
    -> Check if `paint = TRUE` (using `mvBM`) is faster than `slice.tree` and implement by default the faster. + check for equality-ish 
  - [ ] unit test
  - [ ] implement

2 - Go through the following pipeline
## `"chrono.null"`
  * calls `fit.bm` which estimates sig_sq & root_value for each PC axis, using `mvMORPH::mvBM(tree, data, model = "BMM", echo = FALSE, diagnostic = FALSE)`
  * recycle the model part from `run.itsa.model`
  - [ ] unit test
  - [ ] implement

## `sim.counterfactual`
  * calls `sim.counterfactual` which simulates BM from the output of `fit.bm` for each PC axis, according to how many `nsim = n`
  - [ ] unit test
  - [ ] implement

## `run.citsa.model`
  * calls `run.citsa.model` runs `lm` (recycle from model argument) function on the `rbind` `make.deltatronic` + `sim.counterfactual` outputs
  - [ ] unit test
  - [ ] implement

## `area.method`
 * @@@TODO:TG: check if the area implementation can fit with the geiger stuff
 * run `itsa.method`
  - [ ] unit test
  - [ ] implement

## `average.method`
  * takes output from `make.deltatronic`
  * plugs it into optional arg `test = stats::t.test` or `test = stats::aov` etc.
  - [ ] unit test
  - [ ] implement



# 3 - output structure

* Core output: `dispRity object`
  * matrix
  * tree
  * subsets
  * method_output.
  * call
  
method_output expanded:
if (method == "itsa")
* model(s)
if(is_multi){
  * confidence interval of change in slope coefficients
  * confidence interval of immediate jump coefficients
} else {
  * coefficient of change in slope
  * coefficient of immediate jump
}
* convergence issues? 
* if plot(itsa_output){
    shows 
  }

if (method = "citsa")
* model(s)
if(is_multi){
  * confidence interval of change in slope coefficients
  * confidence interval of immediate jump coefficients
} else {
  * coefficient of change in slope
  * coefficient of immediate jump
}
* convergence issues? 




















## Function steps:

1. Calculate disparity object of `empirical_disparity`
2. Plug that into `make.control` function, which outputs `n` replicates as a list of matrices.
3. Make disparity object from the output of 2, becomes `control_disparity`
4. Run CITSA (`ols.deltatronic.R`) to do interrupted time series analysis, inputting `empirical_disparity` & `control_disparity`.
   


## Future directions

* Clade-clade interactions:
  * Density-Dependent trait evolution?
  * incorporate an interaction model into CITSA.
  * estimate how much the presence of dinosaurs restricted the Brownian variance or bounded the morphospace of mammals during the Mesozoic
  * You simulate an alternate Cenozoic timeline where Clade A did not go extinct and continued to occupy that morphospace
  * You project Clade B's counterfactual disparity while still under the "pressure" of Clade A

* Exogenous variable eg temp
  * fit an environment-dependent Brownian Motion model to pre-intervention data.
  * During the Mesozoic, what was the mathematical relationship between global temperature and the rate of morphospace expansion
  * project the simulation forward into the Cenozoic. But instead of just a constant Mesozoic background rate, we use treats function recalculates the variance at every time step based on the actual Cenozoic temperature curve.
  * counterfactual funnel is no longer just a straight, widening cone, it expands and widens with temperatures.

Shift to positon/area of morphospace, eg link to hollowness of traitspace, trend ongoing or is that due to anthropogenic influences.
