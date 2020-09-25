# #' @title Fit ancestral character estimation model
# #'
# #' @description First different models of ancestral character estimation models to a matrix
# #'
# #' @param data A \code{matrix} or \code{list} with the characters for each taxa.
# #' @param tree A \code{phylo} or \code{mutiPhylo} object (if the \code{tree} argument contains node labels, they will be used to name the output).
# #' @param models A \code{vector} of models to be passed to \code{\link[castor]{asr_mk_model}}. If left empty, the it will use the \code{\link{fit.ace.model}} function to find the best model using the first tree. See details.
# #' @param special.tokens optional, a named \code{vector} of special tokens to be passed to \code{\link[base]{grep}} (make sure to protect the character with \code{"\\\\"}). By default \code{special.tokens <- c(missing = "\\\\?", inapplicable = "\\\\-", polymorphism = "\\\\&", uncertainty = "\\\\/")}. Note that \code{NA} values are not compared and that the symbol "@" is reserved and cannot be used.
# #' @param special.behaviours optional, a \code{list} of one or more functions for a special behaviour for \code{special.tokens}. See details.
# #' @param brlen.multiplier optional, a vector of branch length modifiers (e.g. to convert time branch length in changes branch length) or a list of vectors (the same length as \code{tree}).
# #' @param verbose \code{logical}, whether to be verbose (\code{TRUE}) or not (\code{FALSE} - default).
# #' 
# #' @examples
# #' 
# # ' @return
# # ' Returns of \code{list} of:
# # ' \begin{itemize}
# # '  \item \code{"models"}: the best fitted models for each character
# # '  \item \code{"fits"}: the diagnosis tables for each model for each character
# # ' \end{itemize}
# # '
# #' @seealso
# #' 
# #' @author Thomas Guillerme
# #' @export

# fit.ace.model <- function(data, tree, models = c("ER", "SYM", "ARD", "SUEDE", "SRD"), special.tokens,special.behaviours, brlen.multiplier, verbose = FALSE) {

#     ## Run some model fitting.
#     stop("TODO!")


#     all_models <- as.list(models)


#     return(list("models" = best_models, "fits" = all_fits))
# }