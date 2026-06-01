# Road map for the Deltatronic update

This is gonna be integrated in the next release 1.10 (including the `BAT` compatibility + other stuff).

## Functions wish list for Deltratronic

# 1 - user level function `chrono.test`

One function called `chrono.test` that contains the series:

#'@param method, "character" that can be any of the following:
 * `"itsa"`
 * `"citsa"`
 * `"area"`
 Or a function that outputs a `"h.test"` object (e.g. `"t.test"`, or `aov`)

#'@param changepoint, a vector of at least one "numeric", "integer" or "character" `"detect"`
 
#'@param time.window, a vector of 2 "numeric", "integer" that designate the time window around changepoint. OR a single "numeric", "integer" for the number of datapoints before AND after (e.g. time.window = 3, will use three before and three after), OR a "numeric" <0.5, that will be a percentage of the data to integrate before and after (time.window = 0.5 will include all the data)

#'@param ... is a named list of specific arguments for each method

```r
chrono.test(disparity, method, changepoint, time.window, ...)
```

### TODO:

 * [ ] write the doc (with these arguments above)
 * [ ] implement the check.* for these four arguments
 * [ ] do the sanity unit testing (e.g. if `chrono.test(disparity, method = "kista", changepoint, time.window, ...)` error should be: "Wrong method type, the ones allowed are...")



# 2 - under the hood functions

 * `check.time` function for sanitizing both `changepoint` and `time.window` args 

 * `"chrono.test"` data.frame style = `make.deltatronic`

 * `set.changepoint` generating a list of changepoints to go through (each in a absolute time format (e.g. 66)). If only one changepoint, list contains only one element, else more. If `changepoint = "detect"` by default it creates a list using the input time slices, else you can use `changepoint = list(method = "detect", resolution = 2)` which will create a list of every 2 mya (within the time window).

 * `set.time.window`  

 * if method = `"itsa"` calls `"itsa.disparity"`

 * if method = `"citsa"` calls `"chrono.null"` + `"itsa.bm"`
 
 * if method = `"area"` calls `"area.disparity"` @@@TODO:TG: check if the area implementation can fit with the geiger stuff
 
 * if method = `"t.test"` calls `"stats::t.test"`




















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
