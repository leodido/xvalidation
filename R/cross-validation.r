match_method <- function(method = c('kfold', 'holdout', 'loo')) {
  match.arg(method)
}

#' Creates the folds to perform cross-validation.
#'
#' @param dataset a list of elements on which will be performed the partitioning
#' @param method the method of partitioning [default is \code{\link{kfold}}]
#' @param k the number of folds in which partition the list of elements
#' @param names a flag to indicate whether the folds must have names or not
#' @return A list of folds, each of which containing the indices of its train and test set
#' @export
xvalidation <- function(dataset, method = getOption('xvalidation.method'), k = getOption('xvalidation.k'), names = getOption('xvalidation.fold.name'), parallel = FALSE, pos = 1L) {
  # preconditions
  assert_that(is.vector(dataset), is.flag(names), is.numeric(k), is.flag(parallel)) # FIXME: pos type?
  # save the current function
  this <- eval(match.call()[[1L]], parent.frame(n = pos))
  print(this)
  # compute folds
  method <- match_method(method)
  n_obs <- length(dataset)
  partitions <- NULL
  if (method == 'kfold') {
    partitions <- eval(call(method, n_obs, k, names))
  } else {
    partitions <- eval(call(method, n_obs, names))
  }
  #
  train_func <- trainer(this)
  train <- function(partition, fold_id) {
    fold = new.env()
    fold$id = fold_id
    train_func(dataset[partition$train])
  }
  test_func <- validator(this)
  test <- function(model, partition, fold_id) {
    fold = new.env()
    fold$id = fold_id
    test_func(model, dataset[partition$test])
  }
  stats_func <- evaluator(this)
  stats <- function(results, partition, fold_id) {
    fold = new.env()
    fold$id = fold_id
    stats_func(results, dataset[partition$train])
  }
  aggr_func <- aggregator(this)
  #
  iterator <- match.fun(ifelse(parallel, 'mclapply', 'lapply'))
  i <- create_counter(1)
  output <- iterator(partitions, function(fold) {
    if (!is.null(train_func)) {
      # phase 1: computation of the model for the training set of the current fold
      fold[['model']] <- train(fold, i$value())
      if (!is.null(test_func)) {
        # phase 2: inference the test set of the current fold on the model
        fold[['results']] <- test(fold[['model']], fold, i$value())
        # phase 3: compute performance statistics
        if (!is.null(stats_func)) fold[['stats']] <- stats(fold[['results']], fold, i$value())
      }
      i$increment(1)
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
kfold <- function(n_obs, k = getOption('xvalidation.k'), names = getOption('xvalidation.fold.name')) {
  # check preconditions
  assert_that(is_count(n_obs))
  assert_that(is_count(k))
  assert_that(n_obs >= k)
  # compute train and test partitions
  set.seed(k) # NOTE: function became deterministic
  id <- sample(rep(seq.int(k), length.out = n_obs))
  l <- lapply(seq.int(k), function(x) list(train = which(x != id), test = which(x == id)))
  if (names) names(l) <- sapply(seq.int(k), function(i) paste(getOption('xvalidation.fold.prefix'), i, sep = ''))
  # return
  l
}

#' @rdname xvalidation
holdout <- function(n_obs, names = getOption('xvalidation.fold.name')) {
  kfold(n_obs, 2L, names)
}

#' @rdname xvalidation
loo <- function(n_obs, names = getOption('xvalidation.fold.name')) {
  kfold(n_obs, n_obs, names)
}
