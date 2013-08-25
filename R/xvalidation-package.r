#' xvalidation is a set of simple but effective functions to cross-validate your predictive model on your dataset.
#'
#' @section Package options
#'
#' It uses the following \code{\link{options}} to configure behaviour:
#'
#' \itemize{
#'   \item \code{xvalidation.k}: default number of folds
#'
#'   \item \code{xvalidation.method}: a character vector indicating the implemented
#'     partitioning methods. Set it only to change the default behaviour of
#'     \code{\link{xvalidation}}, given by the first element, but do not use other elements
#'     that are not in this set
#'     
#'   \item \code{xvalidation.fold.name}: control whether the folds have to be a named list or not
#'
#'   \item \code{xvalidation.fold.prefix}: specify the prefix to be used for the names of the folding list
#' }
#' @docType package
#' @name xvalidation
#' @import assertthat
NULL

.onLoad <- function(libname, pkgname) {
  ops <- options()
  xvalidation_ops <- list(
    xvalidation.k = 10L,
    xvalidation.method = c('kfold', 'holdout', 'loo'),
    xvalidation.fold.name = TRUE,
    xvalidation.fold.prefix = 'fold_'
  )
  to_set <- !(names(xvalidation_ops) %in% names(ops))
  if (any(to_set)) options(xvalidation_ops[to_set])
  invisible()
}

# TODO: in '<-' function check that x parameter is the xvalidation function
# HOWTO: i suppose matching the standardized signature of x with the standardized signature of xvalidation

# TODO: results parameter have to be a named list with this structure for my multi_class_summary
# predictions: vector of n elements of type c
# actuals: vector of n element of type c
# probability: numeric vector of n element with values between 0 and 1
# n -> number of elements of the validation/test set

# TODO: add (and export) default function/s to evaluate inferencer/classifier performance on single fold stats
# TODO: add (and export) default function/s to aggregate (e.g. micro avg, macro avg, both) single fold stats into one

# TODO: nella prossima versione aggiungere funzioni per
# - logging dei fold creati (indici, nomi degli elementi del dataset corrispondenti, entrambi)
# - serializzazione/salvataggio della configurazione di folding
# - lettura da file preformattati di configurazioni di folding (con controlli: deve essere una cross-validation corretta, chiaramente)


# trainer       N     Y     Y     Y     Y          
# validator     N     N     Y     Y     Y     
# evaluator     N     N     N     Y     Y     
# aggregator    N     N     N     N     Y     
#               1     2     3     4     5
# output:
# 1 -> folding configuration
# 2 -> folding configuration + learned models
# 3 -> folding configuration + learned models + prediction results
# 4 ->          ''           +       ''       +         ''         + stats
# 5 ->          ''           +       ''       +         ''         + stats + aggregated stats
# aggregator -> evaluator -> validator -> trainer
