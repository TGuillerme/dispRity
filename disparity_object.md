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
	|	|	\---$elements* = class:"matrix" (one column matrix containing the elements within the first subsamples)
	|	|	|
	|	|	\---[[2]] = class:"matrix" (matrix containing the bootstrap draws for the unrarefied data)
	|	|	|
	|	|	\---[[3]] = class:"matrix" (matrix containing the bootstrap draws for the first rarefaction level)
	|	|	|
	|	|	\---[[...]] = class:"matrix" (matrix containing the bootstrap draws for the second rarefaction level etc.)
	|	|
	|	\---[[2]] = class:"list" (second item in subsamples list)
	|	|	|
	|	|	\---$elements* = class:"matrix" (one column matrix containing the elements within the second subsamples)
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
	|		\---$elements* = class:"matrix" (a one column matrix containing the elements within this subsamples)
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

### Using `fetch.dispRity` functions for accessing specific data elements (matrix, etc.)

## `fetch.matrix` for accessing a specific ordinated matrix

```{r}
## To extract the matrix for the second subsamples, first rarefaction level and 58th bootstrap
fetch.matrix(dispRity_object, subsamples = 2, rarefaction = 1, bootstrap = 58)
```

## `fetch.elements` to extract the elements within a specific subsamples

```{r}
## To extract the elements in the first subsamples
fetch.elements(dispRity_object, subsamples = 1)
```

## `fetch.subsamples` to extract the subsamples names

```{r}
## To extract the subsamples names
fetch.elements(dispRity_object)
```

> Note that in each case, the values can be set to `0` to fetch the original matrix. For example, `fetch.matrix(dispRity_object, 0, 0, 0)` extracts the original input matrix (or `fetch.matrix(dispRity_object)` by default).

### Using `get.dispRity` or `extract.dispRity` to get the disparity elements (i.e. disparity values)

```{r}
...
```