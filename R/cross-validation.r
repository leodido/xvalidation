match_method <- function(method = c('kfold', 'holdout', 'loo')) {
  match.arg(method)
}

#' Creates the folds to perform cross-validation.
#'
#' @param       dataset     a list of elements on which will be performed the partitioning
#' @param       method      the method of partitioning [default is \code{\link{kfold}}]
#' @param       k           the number of folds in which partition the list of elements
#' @param       names       a flag to indicate whether the folds must have names or not
#' @param       parallel    ...
#' @param       pos         ...
#' @return      A list of folds, each of which containing the indices of its train and test set
#' @export
xvalidation <- function(dataset, method = getOption('xvalidation.method'), k = getOption('xvalidation.k'), names = getOption('xvalidation.fold.name'), parallel = FALSE, pos = 1L) {
  # preconditions
  assert_that(is.vector(dataset), is.flag(names), is.numeric(k), is.flag(parallel)) # FIXME: pos type?
  # save the current function
  this <- eval(match.call()[[1L]], parent.frame(n = pos))
  # compute folds
  method <- match_method(method)
  n_obs <- length(dataset)
  partitions <- NULL
  if (method == 'kfold') {
    partitions <- eval(call(method, n_obs, k, names))
  } else {
    partitions <- eval(call(method, n_obs, names))
  }
  # retrieve and bundle handlers
  preprocess_func <- preprocessor(this)
  preprocess <- function(partition) {
    preprocess_func(partition)
  }
  train_func <- trainer(this)
  train <- function(partition) {
    fold <- new.env()
    fold$id <- partition$id
    train_func(dataset[partition$train])
  }
  test_func <- validator(this)
  test <- function(model, partition) {
    fold <- new.env()
    fold$id <- partition$id
    test_func(model, dataset[partition$test])
  }
  stats_func <- evaluator(this)
  stats <- function(results, partition) {
    fold <- new.env()
    fold$id <- partition$id
    stats_func(results, dataset[partition$train])
  }
  aggr_func <- aggregator(this)
  # create temp file to keep track of iterations completed in a parallelized environment
  if (parallel) {
    tmp_bin_file <- fifo(tempfile(), open = 'w+b', blocking = TRUE)
    # child process
    if (inherits(fork(), 'masterProcess')) {
      progress <- 0.0
      while (progress < 1 && !isIncomplete(tmp_bin_file)) {
          msg <- readBin(tmp_bin_file, 'double')
          progress <- progress + as.numeric(msg)
          cat(sprintf('*** progress: %.2f%% ***\n', progress * 100))
      }
      exit()
    }
  }
  #
  iterator <- match.fun(ifelse(parallel, 'mclapply', 'lapply'))
  output <- iterator(partitions, function(fold) {
    cat(sprintf('current fold ID is %02d.\n', fold$id))
    if (!is.null(preprocess_func)) {
      preprocess(fold)
    }
    if (!is.null(train_func)) {
      # phase 1: computation of the model for the training set of the current fold
      fold[['model']] <- train(fold)
      if (!is.null(test_func)) {
        # phase 2: inference the test set of the current fold on the model
        fold[['results']] <- test(fold[['model']], fold)
        # phase 3: compute performance statistics
        if (!is.null(stats_func)) fold[['stats']] <- stats(fold[['results']], fold)
      }
    }
    # write temp file to communicate that current iteration has ended when parallelized
    if (parallel) {
      writeBin(1 / length(partitions), tmp_bin_file)
    }
    # return
    fold
  })
  # phase 4: aggregation of statistics
  if (!is.null(aggr_func)) output[['aggregated']] <- aggr_func(lapply(output, '[[', 'stats'))
  # close temp binary file (used only when parallelized)
  if (parallel) {
    close(tmp_bin_file)
  }
  # invisible return
  invisible(output)
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
  l <- lapply(seq.int(k), function(x) list(id = x, train = which(x != id), test = which(x == id)))
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
