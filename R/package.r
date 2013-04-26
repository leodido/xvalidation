#' Xvalidation is a set of simple but effective functions to cross-validate your predictive model on your dataset.
#'
#' @section Package options
#'
#' It uses the following \code{\link{options}} to configure behaviour:
#'
#' \itemize{
#'   \item \code{xvalidation.k}: default number of folds
#'
#'   \item \code{xvalidation.methods}: a character vector indicating the implemented
#'     partitioning methods. Set it only to change the default behaviour of
#'     \code{\link{xvalidation}}, given by the first element, but do not use other elements
#'     that are not in this set
#'     
#'   \item \code{xvalidation.foldnames}: control whether the folds have to be a named list or not
#'
#'   \item \code{xvalidation.foldprefix}: specify the prefix to be used for the names of the folding list
#' }
#' @docType package
#' @name xvalidation
NULL

.onLoad <- function(libname, pkgname) {
  ops <- options()
  xvalidation_ops <- list(
    xvalidation.k = 10L,
    xvalidation.methods = c('kfold', 'holdout', 'loo'),
    xvalidation.foldnames = TRUE,
    xvalidation.foldprefix = 'fold_'
  )
  to_set <- !(names(xvalidation_ops) %in% names(ops))
  if (any(to_set)) options(xvalidation_ops[to_set])
  invisible()
}
