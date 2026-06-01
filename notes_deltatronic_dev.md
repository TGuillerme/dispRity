---
title: "Hi there"
---


# Notes for DELTA project 

## Aim - what are we trying to achieve

* Current approaches are based on changes in average across bins; e.g. mass extinction k-pg disparity.
* Need to fit a BACI approach


## Model sketch

### Standardise the trait space
* Coverage-based rarefaction (see Benson paper)
  * Use SQS algorithm?
* Ensures magnitude of change is not driven by number of fossils in each bin.

### Interrupted time series analyses (i.e. segmented linear regression)
* Formulate model where disparity is a function of:
  * Time T
  * Impact I (dummy variable of 0 for before, 1 for after)
  * Interaction (I * T) measures change in gradient
    * So lets say Y^_t0 is the impact point. We fit the model up to this point. we can then simulta

### Handle autocorrelation
* AR1
* GLS with autoregressive term?
  * simple approach is the `AR1` model that uses the residual in the prior time as a predictor of the residual in the next? seems appropriate?

Three nested levels of Bayesian sampling:
* Across posterior of phylgoenetic/ASE uncertainty
* Across model parameters of gls 
* Simulate the future (the forecast cone)

### BACI
* fit trend forecast based on pre-impact data.
  * So lets say Y^_t0 is the impact point. We fit the model up to this point. we can then simulate Y^_t0+x by drawing a new value from the model, given that time has increased and from the residuals of the last point pre-impact.
* Generates the counterfactual
* Calcualte the effect size. 


## Scenarios need to explore
* No change (false positive?)
  * BM
  * OU
  * Early burst
* Change halfway (true positive?)
  * BM -> OU
  * OU -> BM
* Random extinction (false positive)
* Selective extinction



Does it change at time X?
How important is this change (effect size-ish?) - maybe this is the cone of possibility idea
How likely is this change (comparing to a null: option: let's continue the curve unchanged (no dummy var switch) and calcualte the cumulative difference through time); option is a bootstrapping/randomising thingy where you randomise where the switch in dummy variable happens

Perhaps needs to be a controlled intervention time series analysis 
  - i.e. to isolate effects of phylogeny, lineage diversification etc.
  - Simulate disparity with same parameters as estiamted from empirical pre-intervention then see what it does after.



ALSO - in terms of disparity, surely doing a per-taxon disparity score is better than summarising the entire traitspace for through time analyses. Or at least getting a distribution of what it is like.







## Potential caveats

* Do trends in evolution actually exist?
* How do you account for sampling bias
* How do you forecast macroevolutionary trends
  * One idea, is to predict future states in same way we have estimated ancestral states?
    * Model the transition rates
    * Assume they will continue at that rate (unperturbed).
  * Other idea (more realistic) is to forecast the range of disparity curves
* Comes down to a gradual vs punctuated debate
  * If we use ancestral states, we implicitly assume gradual evolution and thus will find gradual evolution.
  * We need to figure out if ancestral states can identify rate heterogeinity. Some clades will be punctuated and recovery with this method may assume gradualism.
  * Another consideration, is how will dispatity change with acutal sample size if forecasting?

## Current software implementations
### Paleo-TS

Quote from Gearty et al.: "Furthermore, while the use of fossil specimens and measurements may give a more complete picture of body size changes through time, the paleoTS software is designed specifically for use with ancestor-descendant sequences and accounts for the auto-correlation of adjacent points in the time series but does not account for the varied dependence of measurements to one another in a single time bin or across time bins"  https://www.pnas.org/doi/full/10.1073/pnas.1712629115#sec-3. 

* Running ancestral state estimation pre-extinction tree slice? So that morphologies are not influenced by the distribution of traits post-extinction






## 17/02 MODEL SKETCH

### LAYER ONE
* We have a phylogeny
* Trait data
* Distribution of ancestral states (say 100)

Each estimation gives us 1 disparity curve, so we have 100 disparity curves

### Layer TWO
* Fit evolutionary model to pre-intervention data
* Extract parameters
* Simulate trait evolution under same process across the phylogeny -> generates counterfactual/control
* Compute disparity through time
* These are our null hypotheses

### LAYER THREE
* We define the structural changes in curves
  * Immediate jump
  * Change in slope
* We get the 95% credible intervals

### LAYER FOUR
* For each ancestral scenario, we ask is the observed intervention effect larger than expected under null evolution
* Average across all ancestral scenarios



## THINGS TO DO
* Modify `fit_BM` function so it can account for ancestral states
*  