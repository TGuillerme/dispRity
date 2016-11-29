### Schematised structure of a `dispRity` object (lite version)

```
object
	|
	\---$matrix* = class:"matrix" (the original ordinated matrix)
	|
	\---$series* = class:"list" (the list of series)
		|
		\---$origin* = class:"list" (the original series containing the original dataset)
		|	|
		|	\--$elements* = class:"numeric" (the vector contianing the elements withing this series)
		|	|
		|	\---[[2]]* = class:"list" (the first rarefaction=n containing the bootstraps)
		|	|	|
		|	|	\---[[1]]* = class:"numeric" (the vector containing the unbootstrapped row numbers)
		|	|	|
		|	|	\---[[2]] = class:"numeric" (a vector containing bootstrapped row numbers)
		|	|	|
		|	|	\---[[...]] = class:"numeric" 
		|	|	|
		|	|	\---[[n]] = class:"numeric" (the nth bootstrap row numbers draw)
		|	|
		|	\---[[3]] = class:"list" (the second rarefaction=n-m containing the bootstraps)
		|	|	|
		|	|	\---[[1]] = class:"numeric" (the vector containing the unbootstrapped row numbers)
		|	|	|
		|	|	\---[[2]] = class:"numeric" (a vector containing bootstrapped and rarefied row numbers)
		|	|	|
		|	|	\---[[...]] = class:"numeric" 
		|	|	|
		|	|	\---[[n]] = class:"numeric" (the nth bootstrap row numbers draw)			
		|	|
		|	\---[[...]] = class:"list" (the following rarefactions)
		|		|
		|		\---[[...]] = class:"numeric" (the bootstraps)
		|
		\---[[2]] = class:"list" (the first series the )
		|	|
		|	\--$elements* = class:"numeric" (the vector contianing the elements withing this series)
		|	|
		|	\---[[2]]* = class:"list" (the first rarefaction=n containing the bootstraps)
		|	|	|
		|	|	\---[[1]]* = class:"numeric" (the vector containing the unbootstrapped row numbers)
		|	|	|
		|	|	\---[[2]] = class:"numeric" (a vector containing bootstrapped row numbers)
		|	|	|
		|	|	\---[[...]] = class:"numeric" 
		|	|	|
		|	|	\---[[n]] = class:"numeric" (the nth bootstrap row numbers draw)
		|	|
		|	\---[[3]] = class:"list" (the second rarefaction=n-m containing the bootstraps)
		|	|	|
		|	|	\---[[1]] = class:"numeric" (the vector containing the unbootstrapped row numbers)
		|	|	|
		|	|	\---[[2]] = class:"numeric" (a vector containing bootstrapped and rarefied row numbers)
		|	|	|
		|	|	\---[[...]] = class:"numeric" 
		|	|	|
		|	|	\---[[n]] = class:"numeric" (the nth bootstrap row numbers draw)			
		|	|
		|	\---[[...]] = class:"list" (the following rarefactions)
		|		|
		|		\---[[...]] = class:"numeric" (the bootstraps)
		|
		\---[[2]] = class:"list" (the first series)
		|	|
		|	\--$elements* = class:"numeric" (the vector contianing the elements withing this series)
		|	|
		|	\---[[2]]* = class:"list" (the first rarefaction=n containing the bootstraps)
		|	|	|
		|	|	\---[[1]]* = class:"numeric" (the vector containing the unbootstrapped row numbers)
		|	|	|
		|	|	\---[[2]] = class:"numeric" (a vector containing bootstrapped row numbers)
		|	|	|
		|	|	\---[[...]] = class:"numeric" 
		|	|	|
		|	|	\---[[n]] = class:"numeric" (the nth bootstrap row numbers draw)
		|	|
		|	\---[[3]] = class:"list" (the second rarefaction=n-m containing the bootstraps)
		|	|	|
		|	|	\---[[1]] = class:"numeric" (the vector containing the unbootstrapped row numbers)
		|	|	|
		|	|	\---[[2]] = class:"numeric" (a vector containing bootstrapped and rarefied row numbers)
		|	|	|
		|	|	\---[[...]] = class:"numeric" 
		|	|	|
		|	|	\---[[n]] = class:"numeric" (the nth bootstrap row numbers draw)			
		|	|
		|	\---[[...]] = class:"list" (the following rarefactions)
		|		|
		|		\---[[...]] = class:"numeric" (the bootstraps)
		|
		\---[[...]] = class:"list" (the followng series)
			|
			\--$elements* = class:"numeric" (the vector contianing the elements withing this series)
			|
			\---[[...]] = class:"list" (the rarefactions)
				|
				\---[[...]] = class:"numeric" (the bootstraps)
```

### Using `fetch.dispRity` functions for accessing specific elements

## `fetch.matrix` for accessing a specific ordinated matrix

```{r}
## To get the matrix of the second series, first rarefaction level and 58th bootstrap
fetch.matrix(dispRity_object, series = 2, rarefaction = 1, bootstrap = 58)
```

## `fetch.elements` to get the elements within a specific series

```{r}
## To get the elements in the first series
fetch.elements(dispRity_object, series = 1)
```

## `fetch.series` to get the series names

```{r}
## To get the series
fetch.elements(dispRity_object)
```

> Note that in each case, the values can be set to `0` to fetch the original matrix. For example, `fetch.matrix(dispRity_object,0,0,0)` gets the original input matrix (or `fetch.matrix(dispRity_object)` by default).