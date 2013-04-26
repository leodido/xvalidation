kfold <- function(n_obs, k = 10L, names = TRUE) {
  # check preconditions
  assert_that(is_count(n_obs))
  assert_that(is_count(k))
  assert_that(n_obs > k)
  # compute train and test partitions
  set.seed(k) # NOTE: function became deterministic
  id <- sample(rep(seq.int(k), length.out = n_obs))
  l <- lapply(seq.int(k), function(x) list(train = which(x != id), test  = which(x == id)))
  if (names) names(l) <- sapply(seq.int(k), function(i) paste('fold_', i, sep = ''))
  # return
  l
}

holdout <- function(n_obs, names = TRUE) kfold(n_obs, k = 2L, names)

loo <- function(n_obs, names = TRUE) kfold(n_obs, k = n_obs, names = TRUE)

xvalidation <- function(data_list, method = c('kfold', 'holdout', 'loo'), k = NULL, names = TRUE) {
  # preconditions
  assert_that(is.vector(data_list))
  assert_that(is.flag(names))
  #
  method <- match.arg(method)
  func <- match.fun(method)
  n_obs <- length(data_list)
  partitions <- NULL
  if (method == 'kfold') {
    if (is.null(k)) k <- as.integer(formals(func)$k)
    partitions <- func(n_obs, k = k, names = names)
  } else {
    partitions <- func(n_obs, names = names)
  }
  # return
  partitions
}
# TODO: add option to return the partition data_list (default demanding this operation to the user)