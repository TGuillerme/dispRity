library(testthat)
library(dispRity)

## Toggles no coverage chunks
# in: test-dispRity.metric; test-dispRity.covar.projections; test-as.covar
nocov <- TRUE 
## Runs the tests
test_check("dispRity")
# test_check("dispRity", reporter = "list")
