### Structure of `dispRity` objects (lite version)

```
object
	|
	\---$matrix* = class:"matrix" (original ordinated matrix)
	|
	\---$call* = class:"list" (details of the methods used)
	|	|
	|	\---$subsets = class:"character"
	|	|
	|	\---$bootstrap = class:"character"
	|	|
	|	\---$dimensions = class:"numeric"
	|	|
	|	\---$metric
	|       |
	|		\---$name = class:"character"
	|		|
	|		\---$fun = class:"list" (elements of class "function")
	|		|
	|		\---$arg = class:"list"
	|
	\---$subsets* = class:"list" (subsets as a list)
	|	|
	|	\---[[1]]* = class:"list" (first item in subsets list)
	|	|	|
	|	|	\---$elements* = class:"matrix" (one column matrix containing the elements within the first subset)
	|	|	|
	|	|	\---[[2]] = class:"matrix" (matrix containing the bootstrap draws for the unrarefied data)
	|	|	|
	|	|	\---[[3]] = class:"matrix" (matrix containing the bootstrap draws for the first rarefaction level)
	|	|	|
	|	|	\---[[...]] = class:"matrix" (matrix containing the bootstrap draws for the second rarefaction level etc.)
	|	|
	|	\---[[2]] = class:"list" (second item in subsets list)
	|	|	|
	|	|	\---$elements* = class:"matrix" (one column matrix containing the elements within the second subset)
	|	|	|
	|	|	\---[[2]] = class:"matrix" (matrix containing the bootstrap draws for the unrarefied data)
	|	|	|
	|	|	\---[[3]] = class:"matrix" (matrix containing the bootstrap draws for the first rarefaction level)
	|	|	|
	|	|	\---[[...]] = class:"matrix" (matrix containing the bootstrap draws for the second rarefaction level etc.)			
	|	|	|
	|	|	\---[[...]] = class:"list" (the following rarefactions)
	|	|	|	|
	|	|		\---[[...]] = class:"numeric" (the bootstraps)
	|	|
	|	\---[[...]] = class:"list" (the following subsets)
	|		|
	|		\---$elements* = class:"matrix" (a one column matrix containing the elements within this subset)
	|		|
	|		\---[[...]] = class:"matrix" (the rarefactions)
	|
	\---$disparity
		|
		\---[[2]] = class:"list" (the first subsets)
		|	|
		|	\---$observed* = class:"numeric" (vector containing the observed disparity within the subsets)
		|	|
		|	\---[[2]] = class:"matrix" (matrix containing the bootstrap draws for the unrarefied data)
		|	|
		|	\---[[3]] = class:"matrix" (matrix containing the bootstrap draws for the first rarefaction level)
		|	|
		|	\---[[...]] = class:"matrix" (matrix containing the bootstrap draws for the second rarefaction level etc.)
		|
		\---[[2]] = class:"list" (the first subsets)
		|	|
		|	\---$observed* = class:"numeric" (vector containing the observed disparity within the subsets)
		|	|
		|	\---[[2]] = class:"matrix" (matrix containing the bootstrap draws for the unrarefied data)
		|	|
		|	\---[[3]] = class:"matrix" (matrix containing the bootstrap draws for the first rarefaction level)
		|	|
		|	\---[[...]] = class:"matrix" (matrix containing the bootstrap draws for the second rarefaction level etc.)			
		|	|
		|	\---[[...]] = class:"list" (the following rarefactions)
		|		|
		|		\---[[...]] = class:"numeric" (the bootstraps)
		|
		\---[[...]] = class:"list" (the following subsets)
			|
			\---$observed* = class:"numeric" (the vector containing the observed disparity within this subsets)
			|
			\---[[...]] = class:"matrix" (the rarefactions)
```
The elements marked with an asterisk (*) are mandatory.