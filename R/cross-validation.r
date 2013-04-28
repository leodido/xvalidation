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
  print(attr(this, 'trainer'))
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
  # do cross validation with the computed folds  or return the fold indices
  if (this %has_attr% 'trainer' && this %has_attr% 'validator') {
    # get the user defined (or default) handlers
    train_func <- trainer(this)
    test_func <- validator(this)
    compute_stats <- FALSE
    if (this %has_attr% 'evaluator') {
      eval_func <- evaluator(this)
      compute_stats <- TRUE
    }
    # for each fold ...
    iterator <- match.fun(ifelse(parallel, 'mclapply', 'lapply'))
    out <- iterator(partitions, function(fold) {
      # train a model on train set
      model <- train_func(dataset[fold$train])
      # validate the model on unseen data (i.e predict unseen observations)
      preds <- test_func(model, dataset[fold$test])
      # compute stats if required or not
      if (compute_stats) {
        eval_func(preds)
      } else {
        list(model = model, results = preds)
      }
    })
    if (this %has_attr% 'aggregator' && compute_stats) {
      aggr_func <- aggregator(this)
      # return the aggregated stats for the current cross-validated simulation
      aggr_func(out)
    } else {
      # two possible returns: a list of performance stats or a list of the model learned and relative prediction results
      # NOTE: the list will have lenght equal to the number of folds used
      out
    }
  } else {
    # return the folds
    partitions
  }
}
# TODO: add option to return the partitioned data_list (default demanding this operation to the user)

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
