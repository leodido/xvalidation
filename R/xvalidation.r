#' Creates the folds to perform cross-validation.
#'
#' @param dataset a list of elements on which will be performed the partitioning
#' @param method the method of partitioning [default is \code{\link{kfold}}]
#' @param k the number of folds in which partition the list of elements
#' @param names a flag to indicate whether the folds must have names or not
#' @return A list of folds, each of which containing the indices of its train and test set
#' @export
xvalidation <- function(dataset, method = getOption('xvalidation.methods'), k = NULL, names = getOption('xvalidation.foldnames')) {
  # preconditions
  assert_that(is.vector(dataset))
  assert_that(is.flag(names))
  #
  method <- match.arg(method)
  func <- match.fun(method)
  n_obs <- length(dataset)
  partitions <- NULL
  if (method == 'kfold') {
    if (is.null(k)) k <- as.integer(eval(formals(func)$k))
    partitions <- func(n_obs, k, names)
  } else {
    partitions <- func(n_obs, names)
  }
  # return
  partitions
}
# TODO: add option to return the partitioned data_list (default demanding this operation to the user)

#' @rdname xvalidation
#' @export
kfold <- function(n_obs, k = getOption('xvalidation.k'), names = getOption('xvalidation.foldnames')) {
  # check preconditions
  assert_that(is_count(n_obs))
  assert_that(is_count(k))
  assert_that(n_obs > k)
  # compute train and test partitions
  set.seed(k) # NOTE: function became deterministic
  id <- sample(rep(seq.int(k), length.out = n_obs))
  l <- lapply(seq.int(k), function(x) list(train = which(x != id), test = which(x == id)))
  if (names) names(l) <- sapply(seq.int(k), function(i) paste(getOption('xvalidation.foldprefix'), i, sep = ''))
  # return
  l
}

#' @rdname xvalidation
#' @export
holdout <- function(n_obs, names = getOption('xvalidation.foldnames')) {
  kfold(n_obs, 2L, names)
}

#' @rdname xvalidation
#' @export
loo <- function(n_obs, names = getOption('xvalidation.foldnames')) {
  kfold(n_obs, n_obs, names)
}
