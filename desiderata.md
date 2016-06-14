# Package wish list
* Add a `k.root` function for scaling at the **k**th root where **k** is the number of ordination axis (useful for scaling product metrics).
* Allow more level 1 functions for metric (e.g. `metric = c(k.root, prod, variances)`).
* Add the possibility to use a `multiPhylo` object in `time.series`.
* A `jackknife.ordination` function for jackknifing the pre-ordination matrix and feed an ordination pipeline (paralel-able and out-of-R-able)

* From Anderson et al 2012 Nature: implement the following in Claddis? :"strength of association between the the 11 coordinate axes (CA) of the NMDS and the biomechanical characters using Spearman rank-order (for continuous characters) and Mann- Whitney U (for the discrete characters) tests".
* Add correlation with sample size for disparity metrics (calculate Spearman’s rho for the correlation between the metric and the sample size)
* Test from Anderson: We applied paired-sample t tests as outlined in Zelditch et al. (2004: 222)
* We also calculated marginal likelihoods for variance as an alternative test for differences in functional disparity between stages. where L is the likelihood, n is sample size, σ2 is the estimated variance parameter, and s2 is the variance
* Changes in disparity between stage bins follows the procedure outlined in Finarelli and Flynn (2007).

