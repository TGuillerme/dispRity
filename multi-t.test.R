## Testing the effect of bootstrap/t.test combination

## Here is an attempt to solve difference testing of bootstrapped distributions
## Basically there are three approaches:
## 1 - Don't bootstrap the data (called "no_bootstrap")
## 2 - Bootstrap the data and compare the whole bootstrap distributions (called "bootstrap"). I.e. for two distributions A and B, and m bootstraps, compare the distribution of {A1, A2, ... Am} to {B1, B2, ... Bm} (1 test)
## 3 - Bootstrap the data but compare each pairs of distributions individually (called "BSdis" for BootStrap DIStribution). I.e. for the two distributions A and B, and m bootstraps, compare the distribution of A1 to B1, of A2 to B2, ... of Am to Bm (m tests).

## The problem of the method 2 is that it multiplies the degree of freedom by m and therefore quickly increases the t statistic and decreases the p-value.

## The problem of method 3 is that it performs m tests and should require correction and will also drastically modify the p-value (multiplying it by m) but not modify the t statistic and the degrees of freedom. One solution to that problem might be to report the 95 CI of the distribution.

## I'm running the tests below

## Loading the functions for testing
test_differences <- function(A, B, correction = "none") {
    ## Calculate the difference, easy peasy.
    difference_no_bootstrap <- t.test(A, B)

    ## Now let's "bootstrap" each distribution 1000 times (here to represent the bootstrap, I replace randomly 1:30 observations in each distribution by a random a random value from it)

    bootstrap <- function(distribution) {
        bootstrap_sampling1 <- sample(1:30, sample(1:30))
        bootstrap_sampling2 <- sample(1:30, length(bootstrap_sampling1))
        distribution[bootstrap_sampling1] <- distribution[bootstrap_sampling2]
        return(distribution)
    }

    A_list <- replicate(1000, bootstrap(A), simplify = FALSE)
    B_list <- replicate(1000, bootstrap(B), simplify = FALSE)

    ## Solution 1: difference between the bootstrapped data
    difference_bootstrap <- t.test(unlist(A_list), unlist(B_list))

    ## Solution 2: distribution of the differences between A and B and distribution of their p-values
    difference_distribution <- mapply(t.test, A_list, B_list, simplify = FALSE)


    ## Now let's extract the results for comparisons

    ## Extracting the t statistic
    difference_no_bootstrap_t <- difference_no_bootstrap$statistic
    difference_bootstrap_t <- difference_bootstrap$statistic
    difference_distribution_t <- apply(difference_distribution, 2, function(X) return(X$statistic))

    ## Extracting the p value
    difference_no_bootstrap_p_value <- difference_no_bootstrap$p.value
    difference_bootstrap_p_value <- difference_bootstrap$p.value
    difference_distribution_p_value <- apply(difference_distribution, 2, function(X) return(X$p.value))
    difference_distribution_p_value <- p.adjust(difference_distribution_p_value, "none")

    ## Extracting the degrees of freedom
    difference_no_bootstrap_df <- difference_no_bootstrap$parameter
    difference_bootstrap_df <- difference_bootstrap$parameter
    difference_distribution_df <- apply(difference_distribution, 2, function(X) return(X$parameter))


    ## Results table
    results <- matrix(NA, 3, 5, dimnames = list(c("t", "p.value", "df"), c("no_bootstrap", "bootstrap", "median.BSdis", "2.5%CI.BSdis", "97.5%CI.BSdis")))

    results[1,] <- c(difference_no_bootstrap_t, difference_bootstrap_t, median(difference_distribution_t), quantile(difference_distribution_t, probs = c(0.025, 0.975)))
    results[2,] <- c(difference_no_bootstrap_p_value, difference_bootstrap_p_value, median(difference_distribution_p_value), quantile(difference_distribution_p_value, probs = c(0.025, 0.975)))
    results[3,] <- c(difference_no_bootstrap_df, difference_bootstrap_df, median(difference_distribution_df), quantile(difference_distribution_df, probs = c(0.025, 0.975)))
        
    ## Return results
    return(results)
}

# set.seed(0)
# ## First test is with two identical and big distributions
# A <- rnorm(100, mean = 0, sd = 1)
# B <- rnorm(100, mean = 0, sd = 1)

# round(test_differences(A, B), digit = 4)
# ## there are no significant differences using the method 1 and 3 but a significant one using the method 2.


# ## Second test is the same but with a smaller distribution
# A <- rnorm(20, mean = 0, sd = 1)
# B <- rnorm(20, mean = 0, sd = 1)

# round(test_differences(A, B), digit = 4)
# ## there are no significant differences using the method 1, 2 and 3.


# ## Third is the same but with different sizes distributions
# A <- rnorm(20, mean = 0, sd = 1)
# B <- rnorm(100, mean = 0, sd = 1)

# round(test_differences(A, B), digit = 4)
# ## there are no significant differences using the method 1 and 3 but a significant one using the method 2.


#Automatic test run
auto.test <- function() {

    run.one.simulation <- function() {
        make.distribution <- function(difference = FALSE, size.difference = FALSE) {
            #size
            if(size.difference == FALSE) {
                size1 <- sample(10:100, 1)
                size2 <- size1
            } else {
                size1 <- sample(10:100, 1)
                size2 <- sample(10:100, 1)
            }

            #difference
            if(difference == FALSE) {
                mean1 <- runif(1)
                sd1 <- runif(1)
                mean2 <- mean1
                sd2 <- sd1
            } else {
                mean1 <- runif(1)
                sd1 <- runif(1)
                mean2 <- runif(1)
                sd2 <- runif(1)
            }

            #Draw the distributions
            return(list(A=rnorm(size1, mean1, sd1), B=rnorm(size2, mean2, sd2)))
        }

        test_differences.list <- function(list) {
            return(test_differences(list$A, list$B))
        }

        extract_parameters <- function(distributions) {
            return(list(
                A = c(length(distributions$A), mean(distributions$A), sd(distributions$A)),
                B = c(length(distributions$B), mean(distributions$B), sd(distributions$B))
                )
            )
        }


        distributions <- make.distribution(sample(c(TRUE,FALSE), 1), sample(c(TRUE,FALSE), 1))
        parameters <- extract_parameters(distributions)
        results <- test_differences.list(distributions)

        return(list(parameters=parameters, results=results))
    }

    return(replicate(1000, run.one.simulation(), simplify = FALSE))
}

test <- auto.test()
save(test, file="test.Rda")


#Extract the results

#Distributions parameters
distr_sizes_diff <- unlist(lapply(test, function(X) X$parameters$A[[1]]/X$parameters$B[[1]]))
distr_means_diff <- unlist(lapply(test, function(X) X$parameters$A[[2]]/X$parameters$B[[2]]))
distr_stdev_diff <- unlist(lapply(test, function(X) X$parameters$A[[3]]/X$parameters$B[[3]]))

#Distributions differences results
#pvalues
pvalues_noBS <- unlist(lapply(test, function(X) X$results[2,1]))
pvalues_Boot <- unlist(lapply(test, function(X) X$results[2,2]))
pvalues_min <- unlist(lapply(test, function(X) X$results[2,4]))
pvalues_max <- unlist(lapply(test, function(X) X$results[2,5]))
#t-stats
tstats_noBS <- unlist(lapply(test, function(X) X$results[1,1]))
tstats_Boot <- unlist(lapply(test, function(X) X$results[1,2]))
tstats_min <- unlist(lapply(test, function(X) X$results[1,4]))
tstats_max <- unlist(lapply(test, function(X) X$results[1,5]))
#df
degfree_noBS <- unlist(lapply(test, function(X) X$results[3,1]))
degfree_Boot <- unlist(lapply(test, function(X) X$results[3,2]))
degfree_min <- unlist(lapply(test, function(X) X$results[3,4]))
degfree_max <- unlist(lapply(test, function(X) X$results[3,5]))


#make into a data.frame
comp_data <- data.frame(row.names = seq(1:1000),
    "SizeDiff" = distr_sizes_diff,
    "MeanDiff" = distr_means_diff,
    "StdvDiff" = distr_stdev_diff,
    "pv_noBS" = pvalues_noBS,
    "pv_Boot" = pvalues_Boot,
    "pv_min" = pvalues_min,
    "pv_max" = pvalues_max,
    "ts_noBS" = tstats_noBS,
    "ts_Boot" = tstats_Boot,
    "ts_min" = tstats_min,
    "ts_max" = tstats_max,
    "df_noBS" = degfree_noBS,
    "df_Boot" = degfree_Boot,
    "df_min" = degfree_min,
    "df_max" = degfree_max)

summary(aov(pv_noBS ~ SizeDiff*MeanDiff*StdvDiff, data = comp_data))
summary(aov(pv_Boot ~ SizeDiff*MeanDiff*StdvDiff, data = comp_data))
summary(aov(pv_min ~ SizeDiff*MeanDiff*StdvDiff, data = comp_data))
summary(aov(pv_max ~ SizeDiff*MeanDiff*StdvDiff, data = comp_data))

summary(aov(ts_noBS ~ SizeDiff*MeanDiff*StdvDiff, data = comp_data))
summary(aov(ts_Boot ~ SizeDiff*MeanDiff*StdvDiff, data = comp_data))
summary(aov(ts_min ~ SizeDiff*MeanDiff*StdvDiff, data = comp_data))
summary(aov(ts_max ~ SizeDiff*MeanDiff*StdvDiff, data = comp_data))

summary(aov(df_noBS ~ SizeDiff*MeanDiff*StdvDiff, data = comp_data))
summary(aov(df_Boot ~ SizeDiff*MeanDiff*StdvDiff, data = comp_data))
summary(aov(df_min ~ SizeDiff*MeanDiff*StdvDiff, data = comp_data))
summary(aov(df_max ~ SizeDiff*MeanDiff*StdvDiff, data = comp_data))