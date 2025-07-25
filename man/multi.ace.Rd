% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi.ace.R
\name{multi.ace}
\alias{multi.ace}
\title{Ancestral states estimations with multiple trees}
\usage{
multi.ace(
  data,
  tree,
  models,
  sample = 1,
  sample.fun = list(fun = runif, param = list(min = min, max = max)),
  threshold = TRUE,
  special.tokens,
  special.behaviours,
  brlen.multiplier,
  verbose = FALSE,
  parallel = FALSE,
  output,
  options.args,
  estimation.details = NULL
)
}
\arguments{
\item{data}{A \code{matrix}, \code{data.frame} or \code{list} with the characters for each taxa. Or a \code{multi.ace} list (see details).}

\item{tree}{A \code{phylo} or \code{mutiPhylo} object (if the \code{tree} argument contains node labels, they will be used to name the output).}

\item{models}{A \code{character} vector, unambiguous named \code{list} or \code{matrix} to be passed as model arguments to \code{castor::asr_mk_model} or \code{ape::ace} (see details).}

\item{sample}{An \code{integer} for the number of matrices to sample per tree (default is \code{1}). See details.}

\item{sample.fun}{If \code{sample > 1}, a named list containing the following elements: \code{fun} the sampling distribution for continuous characters; and \code{param} (optional) a named list of parameters and their estimation function (default is \code{sample.fun = list(fun = runif, param = list(min = min, max = max))}). See details.}

\item{threshold}{Is ignored if \code{sample > 1}, else either a \code{logical} for applying a relative threshold (\code{TRUE} - default) or no threshold (\code{FALSE}) or a \code{numeric} value of the threshold (e.g. 0.95). See details.}

\item{special.tokens}{optional, a named \code{vector} of special tokens to be passed to \code{\link[base]{grep}} (make sure to protect the character with \code{"\\\\"}). By default \code{special.tokens <- c(missing = "\\\\?", inapplicable = "\\\\-", polymorphism = "\\\\&", uncertainty = "\\\\/")}. Note that \code{NA} values are not compared and that the symbol "@" is reserved and cannot be used.}

\item{special.behaviours}{optional, a \code{list} of one or more functions for a special behaviour for \code{special.tokens}. See details.}

\item{brlen.multiplier}{optional, a vector of branch length modifiers (e.g. to convert time branch length in changes branch length) or a list of vectors (the same length as \code{tree}).}

\item{verbose}{\code{logical}, whether to be verbose (\code{TRUE}) or not (\code{FALSE} - default).}

\item{parallel}{Either a \code{logical}, whether to use parallel algorithm (\code{TRUE}) or not (\code{FALSE} - default); or directly an \code{integer} indicating the number of cores to use (note that if \code{parallel = 1}, one core will be used but the parallel integration will still be called).}

\item{output}{optional, see \code{Value} section below.}

\item{options.args}{optional, a named list of options to be passed to function called by \code{castor::asr_mk_model}.}

\item{estimation.details}{optional, whether to also return the details for each estimation as returned by \code{castor::asr_mk_model} or \code{ape::ace}. This argument can be left \code{NULL} (default) or be any combination of the elements returned by \code{castor::asr_mk_model} or \code{ape::ace} (e.g. \code{c("loglikelihood", "transition_matrix", "CI95")}).}
}
\value{
Returns a \code{"matrix"} or \code{"list"} of ancestral states. By default, the function returns the ancestral states in the same format as the input \code{matrix}. This can be changed using the option \code{output = "matrix"} or \code{"list"} to force the class of the output.
To output the combined ancestral states and input, you can use \code{"combined"} (using the input format) or \code{"combined.matrix"} or \code{"combined.list"}.
If using continuous characters only, you can use the output option \code{"dispRity"} to directly output a usable \code{dispRity} object with all trees and all the data (estimated and input).
\emph{NOTE} that if the input data had multiple character types (continuous and discrete) and that \code{"matrix"} or \code{"combined.matrix"} output is requested, the function returns a \code{"data.frame"}.

If using the option \code{output = "multi.ace"}, the function returns a \code{"multi.ace"} object that can be recycled in the \code{multi.ace} function. This can be useful for testing different sampling strategies of threshold methods without having to recalculate all the ancestral trait estimations.
}
\description{
Fast ancestral states estimations run on multiple trees using the Mk model from castor::asr_mk_model.
}
\details{
Depending on the type of characters \code{models} argument can be either:
\itemize{
     \item the name of a single model to apply to all characters (if all characters are discrete or all are continuous); see below for the list of available names. For example \code{models = "ER"} applies the Equal Rates model to all characters (assuming they are all discrete characters).
     \item a vector of model names to apply to different type of characters (see below for the list). For example \code{models = c("ER", "ER", "BM")} applies the Equal Rates model to the two first characters (discrete) and the \code{"BM"} model to the third character (continuous).
     \item a transition \code{"matrix"} to be applied to all characters (if discrete). For example \code{models = matrix(0.2, 2, 2)}.
     \item an single named list of arguments to be applied to all characters by passing it to \code{ape::ace} (if continuous). For example \code{models = list(method = "GLS", corStruct = corBrownian(1, my_tree))}.
     \item an un-ambiguous list of arguments to be passed to either \code{castor::asr_mk_model} (discrete characters) or \code{ape::ace} (continuous characters). For example \code{models = list("char1" = list(transition_matrix = matrix(0.2, 2, 2)), "char2" = list(method = "GLS", corStruct = corBrownian(1, my_tree)))} to be specifically passed to the characters named "char1" and "char2".
}

The available built-in models for discrete characters in \code{castor::asr_mk_model} are:
\itemize{
 \item \code{"ER"} for all equal rates
 \item \code{"SYM"} for symmetric rates
 \item \code{"ARD"} all rates are different
 \item \code{"SUEDE"} equal stepwise transitions (e.g. for meristic/counting characters)
 \item \code{"SRD"} different stepwise transitions 
}
See directly \code{castor::asr_mk_model} for more models.

The available built-in models and methods for continuous characters in \code{ape::ace} are:
\itemize{
 \item \code{"BM"} model: for a default Brownian Motion with the "REML" method
 \item \code{"REML"} method: for a default Brownian Motion with the "REML" method (same as above)    
 \item \code{"ML"} method: for a default Brownian Motion with the "ML" method
 \item \code{"pic"} method: for a default Brownian Motion with the "pic" (least squared) method    
}

\code{special.behaviours} allows to generate a special rule for the \code{special.tokens}. The functions should can take the arguments \code{character, all_states} with \code{character} being the character that contains the special token and \code{all_states} for the character (which is automatically detected by the function). By default, missing data returns and inapplicable returns all states, and polymorphisms and uncertainties return all present states.
\itemize{
     \item{\code{missing = function(x,y) y}}
     \item{\code{inapplicable = function(x,y) y}}
     \item{\code{polymorphism = function(x,y) strsplit(x, split = "\\\\&")[[1]]}}
     \item{\code{uncertainty = function(x,y) strsplit(x, split = "\\\\/")[[1]]}}
}

Functions in the list must be named following the special token of concern (e.g. \code{missing}), have only \code{x, y} as inputs and a single output a single value (that gets coerced to \code{integer} automatically). For example, the special behaviour for the special token \code{"?"} can be coded as: \code{special.behaviours = list(missing = function(x, y) return(NA)} to make ignore the character for taxa containing \code{"?"}. 

When using the parallel option (either through using \code{parallel = TRUE} by using the number of available cores minus on or manually setting the number of cores - e.g. \code{parallel = 5}), the \code{castor::asr_mk_model} function will use the designated number of cores (using the option \code{Nthreads = <requested_number_of_cores>}). Additionally, if the input \code{tree} is a \code{"multiPhylo"} object, the trees will be run in parallel for each number of cores, thus decreasing computation time accordingly (e.g. if 3 cores are requested and \code{tree} contains 12 \code{"phylo"} objects, 4 different \code{"phylo"} objects will be run in parallel on the 3 cores making the calculation around 3 times faster).

When using the default \code{sample = 1}, only one estimation is sampled per tree:
\itemize{
     \item For continuous characters, this estimation is the average estimated ancestral value;
     \item For discrete characters, this estimation is the one calculated using the \code{threshold} option (see details below). 
}
When using \code{sample > 1}, multiple estimations are sampled per tree:
\itemize{
     \item For continuous characters, this estimation is sample from the 95% confidence interval using the sampling probability function provided by \code{sample.fun}. By default (\code{runif}), the function samples from a uniform bounded by the 95% confidence interval estimation (see below for modifications).;
     \item For discrete characters, the estimations are sampled using their scaled likelihood.
}

The \code{threshold} option allows to convert ancestral states likelihoods into discrete states. When \code{threshold = FALSE}, the ancestral state estimated is the one with the highest likelihood (or at random if likelihoods are equal). When \code{threshold = TRUE}, the ancestral state estimated are all the ones that are have a scaled likelihood greater than the maximum observed scaled likelihood minus the inverse number of possible states (i.e. \code{select_state >= (max(likelihood) - 1/n_states)}). This option makes the threshold selection depend on the number of states (i.e. if there are more possible states, a lower scaled likelihood for the best state is expected). Finally using a numerical value for the threshold option (e.g. \code{threshold = 0.95}) will simply select only the ancestral states estimates with a scaled likelihood equal or greater than the designated value. This option makes the threshold selection absolute. Regardless, if more than one value is select, the uncertainty token (\code{special.tokens["uncertainty"]}) will be used to separate the states. If no value is selected, the uncertainty token will be use between all observed characters (\code{special.tokens["uncertainty"]}).

The \code{sample.fun} option allows to specify a function and parameters for the sampling of the continuous traits. The default is \code{sample.fun = list(fun = runif, param = list(min = min, max = max))} for applying a random uniform sampling (\code{runif}) with the parameters (the minimum and the maximum are applied using respectively the \code{min} and \code{max} functions on the estimated data). For applying different samplings to different traits, you can use a list of arguments in the sample format as \code{sample.fun} (e.g. \code{sample.fun = list(trait_uniform = list(fun = runif, param = list(min = min, max = max)), trait_normal = list(fun = rnorm, param = list(mean = mean, sd = function(x)return(diff(range(x))/4)))} - here the standard deviation is calculated as a quarter of the 95\% CI range).

It is also possible to just run the estimations without sampling but for sampling later (i.e. just run the estimations; save them as a \code{multi.ace} object and then rerun them into the function). You can do that by using the option \code{output = "multi.ace"}. Using this option ignores the following options: \code{sample}, \code{sample.fun}, and \code{estimation.details}.
}
\examples{
set.seed(42)
## A simple example:
## A random tree with 10 tips
tree <- rcoal(10)
## Setting up the parameters
my_rates = c(rgamma, rate = 10, shape = 5)

## A random Mk matrix (10*50)
matrix_simple <- sim.morpho(tree, characters = 50, model = "ER", rates = my_rates,
                            invariant = FALSE)

## Run a basic ancestral states estimations
ancestral_states <- multi.ace(matrix_simple, tree)
ancestral_states[1:5, 1:5]

## A random continuous trait matrix
matrix_continuous <- space.maker(elements = 10, dimensions = 2,
                                 distribution = rnorm, 
                                 elements.names = tree$tip.label)
## A basic estimations for discrete data
ancestral_traits <- multi.ace(matrix_continuous, tree)

## A more complex example for discrete data
## Create a multiple list of 5 trees
multiple_trees <- rmtree(5, 10)

## Modify the matrix to contain missing and special data
matrix_complex <- matrix_simple
matrix_complex[sample(1:length(matrix_complex), 50)] <- "-"
matrix_complex[sample(1:length(matrix_complex), 50)] <- "0\%2"
matrix_complex[sample(1:length(matrix_complex), 50)] <- "?"
matrix_complex[1:5,1:5]

## Set a list of extra special tokens
my_spec_tokens <- c("weirdtoken" = "\%")

## Set some special behaviours for the "weirdtoken" and for "-" and "?"
my_spec_behaviours <- list()
## Inapplicable tokens "-" are ignored
my_spec_behaviours$inapplicable <- function(x,y) return(NA)
## Missing tokens "?" are considered as all states
my_spec_behaviours$missing      <- function(x,y) return(y)
## Weird tokens are considered as state 0 and 3
my_spec_behaviours$weirdtoken   <- function(x,y) return(c(1,2))

## Create a random branch length modifier to apply to each tree
branch_lengths <- rnorm(18)^2

## Setting a list of model ("ER" for the 25 first characters and then "SYM")
my_models <- c(rep("ER", 25), rep("SYM", 25))

## Run the ancestral states on all the tree with multiple options
ancestral_states <- multi.ace(matrix_complex, multiple_trees,
                              verbose = TRUE,
                              models = my_models,
                              threshold = 0.95,
                              special.tokens = my_spec_tokens,
                              special.behaviours = my_spec_behaviours,
                              brlen.multiplier = branch_lengths,
                              output = "combined.matrix")

## The results for the the two first characters for the first tree
ancestral_states[[1]][, 1:2]

\dontrun{
## The same example but running in parallel
ancestral_states <- multi.ace(matrix_complex, multiple_trees,
                              verbose = TRUE,
                              models = my_models,
                              threshold = 0.95,
                              special.tokens = my_spec_tokens,
                              special.behaviours = my_spec_behaviours,
                              brlen.multiplier = branch_lengths,
                              output = "combined.matrix",
                              parallel = TRUE)
}

## Saving your estimations as a "multi.ace" object
ancestor_estimates <- multi.ace(matrix_simple, multiple_trees, output = "multi.ace")

## Estimating ancestral states by sampling distributions
## Discrete characters:
sampled_ancestors <- multi.ace(ancestor_estimates, sample = 25)

## Continuous characters:
## Sampling 100 matrices from the ancestral estimations
sampled_traits <- multi.ace(matrix_continuous, tree, sample = 100)
## Setting up two specific sampling functions
sample_fun <- list(
     unif_sampl = list(fun = runif,
                       param = list(min = min, max = max)),
     norm_sampl = list(fun = rnorm,
                       param = list(mean = mean,
                                    sd = function(x)return(diff(range(x))/4)))
)
## Sampling 100 matrices with different distribution functions
sampled_traits <- multi.ace(matrix_continuous, tree, sample = 100,
                            sample.fun = sample_fun)

}
\seealso{
\code{char.diff}, \code{castor::asr_mk_model}, \code{ape::ace}
}
\author{
Thomas Guillerme
}
