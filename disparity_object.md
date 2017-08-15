### Structure of `dispRity` objects (lite version)

```
object
	|
	\---$matrix* = class:"matrix" (original ordinated matrix)
	|
	\---$call* = class:"list" (details of the methods used)
	|	|
	|	\---$subsamples = class:"character"
	|	|
	|	\---$bootstrap = class:"character"
	|	|
	|	\---$dimensions = class:"numeric"
	|	|
	|	\---$metric = class:"character"
	|
	\---$subsamples* = class:"list" (subsamples as a list)
	|	|
	|	\---[[1]]* = class:"list" (first item in subsamples list)
	|	|	|
	|	|	\---$elements* = class:"matrix" (one column matrix containing the elements within the first subsample)
	|	|	|
	|	|	\---[[2]] = class:"matrix" (matrix containing the bootstrap draws for the unrarefied data)
	|	|	|
	|	|	\---[[3]] = class:"matrix" (matrix containing the bootstrap draws for the first rarefaction level)
	|	|	|
	|	|	\---[[...]] = class:"matrix" (matrix containing the bootstrap draws for the second rarefaction level etc.)
	|	|
	|	\---[[2]] = class:"list" (second item in subsamples list)
	|	|	|
	|	|	\---$elements* = class:"matrix" (one column matrix containing the elements within the second subsample)
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
	|	\---[[...]] = class:"list" (the following subsamples)
	|		|
	|		\---$elements* = class:"matrix" (a one column matrix containing the elements within this subsample)
	|		|
	|		\---[[...]] = class:"matrix" (the rarefactions)
	|
	\---$disparity
		|
		\---[[2]] = class:"list" (the first subsamples)
		|	|
		|	\---$observed* = class:"numeric" (vector containing the observed disparity within the subsamples)
		|	|
		|	\---[[2]] = class:"matrix" (matrix containing the bootstrap draws for the unrarefied data)
		|	|
		|	\---[[3]] = class:"matrix" (matrix containing the bootstrap draws for the first rarefaction level)
		|	|
		|	\---[[...]] = class:"matrix" (matrix containing the bootstrap draws for the second rarefaction level etc.)
		|
		\---[[2]] = class:"list" (the first subsamples)
		|	|
		|	\---$observed* = class:"numeric" (vector containing the observed disparity within the subsamples)
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
		\---[[...]] = class:"list" (the following subsamples)
			|
			\---$observed* = class:"numeric" (the vector containing the observed disparity within this subsamples)
			|
			\---[[...]] = class:"matrix" (the rarefactions)
```
The elements marked with an asterisk (*) are mandatory.