library(testthat)
library(dispRity)

## Toggles no coverage chunks
# nocov <- TRUE in: test-dispRity.metric; test-dispRity.covar.projections; test-as.covar
## Runs the tests
test_check("dispRity")
# test_check("dispRity", reporter = "list")
