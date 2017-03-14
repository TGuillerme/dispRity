### Structure of `dispRity` objects (lite version)

```
object
	|
	\---$matrix* = class:"matrix" (original ordinated matrix)
	|
	\---$call* = class:"list" (details of the methods used)
	|	|
	|	\---$series = class:"character"
	|	|
	|	\---$bootstrap = class:"character"
	|	|
	|	\---$dimensions = class:"numeric"
	|	|
	|	\---$metric = class:"character"
	|
	\---$series* = class:"list" (series as a list)
	|	|
	|	\---[[1]]* = class:"list" (first item in series list)
	|	|	|
	|	|	\---$elements* = class:"matrix" (one column matrix containing the elements within the first series)
	|	|	|
	|	|	\---[[2]] = class:"matrix" (matrix containing the bootstrap draws for the unrarefied data)
	|	|	|
	|	|	\---[[3]] = class:"matrix" (matrix containing the bootstrap draws for the first rarefaction level)
	|	|	|
	|	|	\---[[...]] = class:"matrix" (matrix containing the bootstrap draws for the second rarefaction level etc.)
	|	|
	|	\---[[2]] = class:"list" (second item in series list)
	|	|	|
	|	|	\---$elements* = class:"matrix" (one column matrix containing the elements within the second series)
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
	|	\---[[...]] = class:"list" (the following series)
	|		|
	|		\---$elements* = class:"matrix" (a one column matrix containing the elements within this series)
	|		|
	|		\---[[...]] = class:"matrix" (the rarefactions)
	|
	\---$disparity
		|
		\---[[2]] = class:"list" (the first series)
		|	|
		|	\---$observed* = class:"numeric" (vector containing the observed disparity within the series)
		|	|
		|	\---[[2]] = class:"matrix" (matrix containing the bootstrap draws for the unrarefied data)
		|	|
		|	\---[[3]] = class:"matrix" (matrix containing the bootstrap draws for the first rarefaction level)
		|	|
		|	\---[[...]] = class:"matrix" (matrix containing the bootstrap draws for the second rarefaction level etc.)
		|
		\---[[2]] = class:"list" (the first series)
		|	|
		|	\---$observed* = class:"numeric" (vector containing the observed disparity within the series)
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
		\---[[...]] = class:"list" (the following series)
			|
			\---$observed* = class:"numeric" (the vector containing the observed disparity within this series)
			|
			\---[[...]] = class:"matrix" (the rarefactions)
```
The elements marked with an asterisk (*) are mandatory.

### Using `fetch.dispRity` functions for accessing specific data elements (matrix, etc.)

## `fetch.matrix` for accessing a specific ordinated matrix

```{r}
## To extract the matrix for the second series, first rarefaction level and 58th bootstrap
fetch.matrix(dispRity_object, series = 2, rarefaction = 1, bootstrap = 58)
```

## `fetch.elements` to extract the elements within a specific series

```{r}
## To extract the elements in the first series
fetch.elements(dispRity_object, series = 1)
```

## `fetch.series` to extract the series names

```{r}
## To extract the series names
fetch.elements(dispRity_object)
```

> Note that in each case, the values can be set to `0` to fetch the original matrix. For example, `fetch.matrix(dispRity_object, 0, 0, 0)` extracts the original input matrix (or `fetch.matrix(dispRity_object)` by default).

### Using `get.dispRity` or `extract.dispRity` to get the disparity elements (i.e. disparity values)

```{r}
...
```