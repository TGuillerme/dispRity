######
# Applying this to disparity
######
# Now let say we want to calculate the disparity

library(dispRity)

# First let's generate two dummy matrices:
## Generating a dummy ordinated matrix
ordinated_matrix <- matrix(data = rnorm(180), nrow = 20, ncol = 9,
     dimnames = list(letters[1:20]))
## Creating a list of dummy factors (1 or 2)
factors <- as.data.frame(matrix(data = c(rep(1,10), rep(2,10)), nrow = 20,
     ncol = 1, dimnames = list(letters[1:20])))
## Splitting the dummy ordinated matrix
custom_series <- cust.series(ordinated_matrix, factors)

matrix1 <- custom_series$data$V1.1
matrix2 <- custom_series$data$V1.2

# now we can calculate some distribution of disparity like the distances from centroids
disparity1 <- centroids(matrix1)
disparity2 <- centroids(matrix2)

# One way to look at disparity can be to look a the median distances from centroids and compare them
median(disparity1) - median(disparity2)
# And that gives us our difference.
# The problem is that it's not really statsy. The otherway to look at it could be to look at the difference of the distributions:
t.test(disparity1, disparity2)

#~~~~~~~
# That gives us no difference between our two random disparities (as expected from a random normal distribution!)
# Now the problem here is that we know we have the TRUE answer (simulations!). In reality, and especially with palaeontological data, we might have issues with the data set such as bad sampling, outliers, etc...
# One way to solve these issues is to pseudo-replicate (bootstrap) the data so that our results are less reliable on errors.
#~~~~~~~

# Now looking at the same logic as before, we can bootstrap the matrices
bootstrapped_matrices <- boot.matrix(custom_series)

matrices1 <- bootstrapped_matrices$data$bootstraps$V1.1[[1]]
matrices2 <- bootstrapped_matrices$data$bootstraps$V1.2[[1]]

# now we can calculate disparity the same way
disparity1 <- lapply(matrices1, centroids)
disparity2 <- lapply(matrices2, centroids)

# One way to look at it would be to calculate the distribution of the median distances (i.e. 1000 medians of disparity1 vs 1000 medians of disparity2)
t.test(unlist(lapply(disparity1, median)), unlist(lapply(disparity2, median)))

# The other way is to compare the distributions (to comply with the idea of comparing the distributions, not the central tendencies)
t.test(unlist(disparity1), unlist(disparity2))

#~~~~~~~
# In both cases, it's telling us that the distributions are different (even if we know they're not!).
# This is due to the fact that we're increasing the degrees of freedom something like 1000 times more and thus the t statistic and the p-value will automatically decrease.
# Now, in fairness, we're getting also a bit away from the first question of the difference between our two distributions because in each test we are testing the difference of the sum of our pseudo-replications which is a bit different.
# One way to look at it would be to compare all our pseudo-replicates to each other.
#~~~~~~~

# running all the t-tests
t.tests <- mapply(t.test, disparity1, disparity2)

# extracting the p-values (just for the sake of the argument, the same can be done about the t-statistic and the df of course!)
p.values <- apply(t.tests, 2, function(X) return(X$p.value))

# We can then either represent our median p-value
median(p.values)

# Or actually more correctly using correction
median(p.adjust(p.values, method = "bonferroni"))

#~~~~~~~
# In both cases, it's telling us that the distributions are not different (which is expected).
# One problem though is that the p-value correction is doing the same artefact as for comparing directly the two bootstrapped distributions (see above) but is basically increase the p.value 1000 times (which is problematic in the other way!).
# P.value correction should be used when testing multiple hypothesis though.
# One thing here is that we are still testing ONE hypothesis (is disparity1 different than disparity2) but multiple times.
# One way to report the results might be to just report the quantiles of our multiple test replicates.
#~~~~~~~

# Calculating the 95% CI of the p.values distribution
quantile(p.values, probs = c(0.025, 0.975))

#~~~~~~~
# In this case (depending on the random seed I guess) you still have no difference between the two disparity values but you now have a range of p.values.
#~~~~~~~


# TEST WITH SPACE.MAKER
