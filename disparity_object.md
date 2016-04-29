### Schematised structure of a `dispRity` object


````
object
  |
  \---$data
  |     \---$observed
  |     |        \---[[1_series]] (the first series)
  |     |        |        \---[[1_rarefaction]] (contains a single element)
  |     |        |                \---[[1_bootstrap]] (contains a single element)
  |     |        |                         \---[[1]] an ordinated "matrix"
  |     |        \---[[..._series]]
  |     |        |        \---[[1_rarefaction]] (contains a single element)
  |     |        |                \---[[1_bootstrap]] (contains a single element)
  |     |        |                         \---[[1]] an ordinated "matrix"
  |     |        \---[[n_series]] ... (the nth series)
  |     |                 \---[[1_rarefaction]] (contains a single element)
  |     |                         \---[[1_bootstrap]] (contains a single element)
  |     |                                  \---[[1]] an ordinated "matrix"
  |     \---$bootstrapped (is NULL if data is not bootstrapped)
  |              \---[[1_series]] (the first series)
  |              |        \---[[1_rarefaction]] (the first rarefaction level)
  |              |        |       \---[[1_bootstrap]] (the first bootstrap)
  |              |        |       |        \---[[1]] an ordinated "matrix"
  |              |        |       \---[[..._bootstrap]] 
  |              |        |               \---[[1]] 
  |              |        |       \---[[n_bootstrap]] 
  |              |        |                \---[[1]]
  |              |        \---[[..._rarefaction]] (contains a single element)
  |              |        |       \---[[1_bootstrap]]
  |              |        |       |        \---[[1]]
  |              |        |       \---[[..._bootstrap]] 
  |              |        |               \---[[1]] 
  |              |        |       \---[[n_bootstrap]] 
  |              |        |                \---[[1]]
  |              |        \---[[n_rarefaction]] (the nth rarefaction level a single element)
  |              |                \---[[1_bootstrap]]
  |              |                |        \---[[1]]
  |              |                \---[[..._bootstrap]] 
  |              |                         \---[[1]] 
  |              |                \---[[n_bootstrap]] 
  |              |                         \---[[1]] 
  |              \---[[..._series]]
  |              |        \---[[1_rarefaction]] (the first rarefaction level)
  |              |        |       \---[[1_bootstrap]] (the first bootstrap)
  |              |        |       |        \---[[1]] an ordinated "matrix"
  |              |        |       \---[[..._bootstrap]] 
  |              |        |               \---[[1]] 
  |              |        |       \---[[n_bootstrap]] 
  |              |        |                \---[[1]]
  |              |        \---[[..._rarefaction]]
  |              |        |       \---[[1_bootstrap]]
  |              |        |       |        \---[[1]]
  |              |        |       \---[[..._bootstrap]] 
  |              |        |               \---[[1]] 
  |              |        |       \---[[n_bootstrap]] 
  |              |        |                \---[[1]]
  |              |        \---[[n_rarefaction]] (the nth rarefaction level a single element)
  |              |                \---[[1_bootstrap]]
  |              |                |        \---[[1]]
  |              |                \---[[..._bootstrap]] 
  |              |                         \---[[1]] 
  |              |                \---[[n_bootstrap]] 
  |              |                         \---[[1]] 
  |              \---[[n_series]] ... (the nth series)
  |                       \---[[1_rarefaction]]
  |                       |       \---[[1_bootstrap]]
  |                       |       |        \---[[1]]
  |                       |       \---[[..._bootstrap]] 
  |                       |               \---[[1]] 
  |                       |       \---[[n_bootstrap]] 
  |                       |                \---[[1]]
  |                       \---[[..._rarefaction]]
  |                       |       \---[[1_bootstrap]]
  |                       |       |        \---[[1]]
  |                       |       \---[[..._bootstrap]] 
  |                       |               \---[[1]] 
  |                       |       \---[[n_bootstrap]] 
  |                       |                \---[[1]]
  |                       \---[[n_rarefaction]]
  |                               \---[[1_bootstrap]]
  |                               |        \---[[1]]
  |                               \---[[..._bootstrap]] 
  |                                        \---[[1]] 
  |                               \---[[n_bootstrap]] 
  |                                        \---[[1]] 
  \---$disparity
  |     \---$observed
  |     |        \---[[1_series]] (the first series)
  |     |        |        \---[[1_rarefaction]] (contains a single element)
  |     |        |                \---[[1_bootstrap]] (contains a single element)
  |     |        |                         \---[[1]] "numeric" value or vector (of the unique bootstrap)
  |     |        \---[[..._series]]
  |     |        |        \---[[1_rarefaction]] (contains a single element)
  |     |        |                \---[[1_bootstrap]] (contains a single element)
  |     |        |                         \---[[1]] "numeric" value or vector (of the unique bootstrap)
  |     |        \---[[n_series]] ... (the nth series)
  |     |                 \---[[1_rarefaction]] (contains a single element)
  |     |                         \---[[1_bootstrap]] (contains a single element)
  |     |                                  \---[[1]] "numeric" value or vector (of the unique bootstrap)
  |     \---$bootstrapped (is NULL if data is not bootstrapped)
  |              \---[[1_series]] (the first series)
  |              |        \---[[1_rarefaction]] (the first rarefaction level)
  |              |        |       \---[[1_bootstrap]] (the first bootstrap)
  |              |        |       |        \---[[1]] "numeric" value or vector (of the unique bootstrap)
  |              |        |       \---[[..._bootstrap]] 
  |              |        |               \---[[1]] 
  |              |        |       \---[[n_bootstrap]] 
  |              |        |                \---[[1]]
  |              |        \---[[..._rarefaction]] (contains a single element)
  |              |        |       \---[[1_bootstrap]]
  |              |        |       |        \---[[1]]
  |              |        |       \---[[..._bootstrap]] 
  |              |        |               \---[[1]] 
  |              |        |       \---[[n_bootstrap]] 
  |              |        |                \---[[1]]
  |              |        \---[[n_rarefaction]] (the nth rarefaction level a single element)
  |              |                \---[[1_bootstrap]]
  |              |                |        \---[[1]]
  |              |                \---[[..._bootstrap]] 
  |              |                         \---[[1]] 
  |              |                \---[[n_bootstrap]] 
  |              |                         \---[[1]] 
  |              \---[[..._series]]
  |              |        \---[[1_rarefaction]] (the first rarefaction level)
  |              |        |       \---[[1_bootstrap]] (the first bootstrap)
  |              |        |       |        \---[[1]] "numeric" value or vector (of the unique bootstrap)
  |              |        |       \---[[..._bootstrap]] 
  |              |        |               \---[[1]] 
  |              |        |       \---[[n_bootstrap]] 
  |              |        |                \---[[1]]
  |              |        \---[[..._rarefaction]]
  |              |        |       \---[[1_bootstrap]]
  |              |        |       |        \---[[1]]
  |              |        |       \---[[..._bootstrap]] 
  |              |        |               \---[[1]] 
  |              |        |       \---[[n_bootstrap]] 
  |              |        |                \---[[1]]
  |              |        \---[[n_rarefaction]] (the nth rarefaction level a single element)
  |              |                \---[[1_bootstrap]]
  |              |                |        \---[[1]]
  |              |                \---[[..._bootstrap]] 
  |              |                         \---[[1]] 
  |              |                \---[[n_bootstrap]] 
  |              |                         \---[[1]] 
  |              \---[[n_series]] ... (the nth series)
  |                       \---[[1_rarefaction]]
  |                       |       \---[[1_bootstrap]]
  |                       |       |        \---[[1]]
  |                       |       \---[[..._bootstrap]] 
  |                       |               \---[[1]] 
  |                       |       \---[[n_bootstrap]] 
  |                       |                \---[[1]]
  |                       \---[[..._rarefaction]]
  |                       |       \---[[1_bootstrap]]
  |                       |       |        \---[[1]]
  |                       |       \---[[..._bootstrap]] 
  |                       |               \---[[1]] 
  |                       |       \---[[n_bootstrap]] 
  |                       |                \---[[1]]
  |                       \---[[n_rarefaction]]
  |                               \---[[1_bootstrap]]
  |                               |        \---[[1]]
  |                               \---[[..._bootstrap]] 
  |                                        \---[[1]] 
  |                               \---[[n_bootstrap]] 
  |                                        \---[[1]] 
  \---$elements
  |     \---[[n_elements]] "character" strings (the names of the elements)
  |             \---[[1]] "character" string (the name of the 1st element)
  |             \---[[ ]] ...
  |             \---[[n]] "character" string (the name of the nst element)
  |
  \---$series
  |     \---[[n_series]] "character" or "numeric" string(s) (the name of the series)
  |             \---[[1]] "character" string (the name of the 1st series)
  |             \---[[ ]] ...
  |             \---[[n]] "character" string (the name of the nst series)
  \---$call
        \---[[1]] "character" string
  
````
  
