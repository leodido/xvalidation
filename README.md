xvalidation
===========

Provides a set of **simple** and **useful** `R` functions to perform the **cross-validation** of your model on your dataset.

You can uses it to simply folding your dataset through `xvalidation(dataset, method, k, ...)`.

The folding available methods are:

* `kfold` (note that `k` is `10` by default), to perform a k-fold cross-validation
* `holdout`, to performa a holdout cross-validation
* `loo`, to perform a leave-one-out cross-validation

Or you can completely perform a cross-validated simulation. In this regard it provides useful hooks to set your functions:

1. `preprocessor` to perform preliminary steps (e.g., write the current fold configuration to file)
2. `trainer` to write your learning algorithm
3. `validator` to write your prediction algorithm
4. `evaluator` to compute the performance statistics
5. `aggregator` to average or generally aggregate the performance statistics of each fold

Each of these handlers require a function with a well-defined signature that must be respected (see [example](#a-little-stupid-and-pointless-demo)).

Moreover implementing hooks **2**, **3**, and **4** you can inherit an environment (e.g, `get('fold', parent.frame())`).

`xvalidator` will take care of calling these hooks in the right way for each fold (with the exception of `aggregator` that is called at the end of the simulation).

## How to install

Note that `xvalidation` is not yet available on CRAN, but you can install it and its dependecies with [devtools](https://github.com/hadley/devtools):

```R
library(devtools)
install_github('assertthat', username = 'hadley')
install_github('xvalidation', username = 'leodido')
```

## A little stupid and pointless demo

```R
dataset <- setNames(as.list(sample(letters[1:23], 12, replace = T)), as.character(1:12L))
trainer(xvalidation) <- function(training_set) {
  length(training_set)
}
validator(xvalidation) <- function(model, validation_set) {
  sample(letters[1:23], length(validation_set), replace = T)
}
evaluator(xvalidation) <- function(results, training_set) {
  vect <- results == training_set
  unlist(lapply(split(vect, f = vect), function(i) length(i) / length(vect)))
}
aggregator(xvalidation) <- function(stats) {
  library(plyr)
  mm <- plyr::rbind.fill.matrix(lapply(stats, function(i) data.frame(t(i))))
  mm[is.na(mm)] <- 0
  colSums(mm) / nrow(mm)
}
xvalidation(dataset, k = 4)
```
