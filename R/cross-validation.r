#' Creates the folds to perform cross-validation.
#'
#' @param dataset a list of elements on which will be performed the partitioning
#' @param method the method of partitioning [default is \code{\link{kfold}}]
#' @param k the number of folds in which partition the list of elements
#' @param names a flag to indicate whether the folds must have names or not
#' @return A list of folds, each of which containing the indices of its train and test set
#' @export
xvalidation <- function(dataset, method = getOption('xvalidation.methods'), k = NULL, names = getOption('xvalidation.foldnames'), parallel = FALSE, pos = 1L) {
  # preconditions
  assert_that(is.vector(dataset))
  assert_that(is.flag(names))
  # save the current function
  this <- eval(match.call()[[1L]], parent.frame(n = pos))
  # compute folds
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
  #
  train_func <- trainer(this)
  train <- function(fold) train_func(dataset[fold$train])
  test_func <- validator(this)
  test <- function(model, fold) test_func(model, dataset[fold$test])
  stats_func <- evaluator(this)
  stats <- function(results, fold) stats_func(results, dataset[fold$train])
  aggr_func <- aggregator(this)
  #
  iterator <- match.fun(ifelse(parallel, 'mclapply', 'lapply'))
  output <- iterator(partitions, function(fold) {
    if (!is.null(train_func)) {
      # phase 1: computation of the model for the training set of the current fold
      fold[['model']] <- train(fold)
      if (!is.null(test_func)) {
        # phase 2: inference the test set of the current fold on the model
        fold[['results']] <- test(fold[['model']], fold)
        # phase 3: compute performance statistics
        if (!is.null(stats_func)) fold[['stats']] <- stats(fold[['results']], fold)
      }
      fold
    } else {
      fold
    }
  })
  # phase 4: aggregation of statistics
  if (!is.null(aggr_func)) output[['aggregated']] <- aggr_func(lapply(output, '[[', 'stats'))
  # return
  output
}

#' @rdname xvalidation
kfold <- function(n_obs, k = getOption('xvalidation.k'), names = getOption('xvalidation.foldnames')) {
  # check preconditions
  assert_that(is_count(n_obs))
  assert_that(is_count(k))
  assert_that(n_obs >= k)
  # compute train and test partitions
  set.seed(k) # NOTE: function became deterministic
  id <- sample(rep(seq.int(k), length.out = n_obs))
  l <- lapply(seq.int(k), function(x) list(train = which(x != id), test = which(x == id)))
  if (names) names(l) <- sapply(seq.int(k), function(i) paste(getOption('xvalidation.foldprefix'), i, sep = ''))
  # return
  l
}

#' @rdname xvalidation
holdout <- function(n_obs, names = getOption('xvalidation.foldnames')) {
  kfold(n_obs, 2L, names)
}

#' @rdname xvalidation
loo <- function(n_obs, names = getOption('xvalidation.foldnames')) {
  kfold(n_obs, n_obs, names)
}
