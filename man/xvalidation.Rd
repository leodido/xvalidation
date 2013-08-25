\docType{package}
\name{xvalidation}
\alias{holdout}
\alias{kfold}
\alias{loo}
\alias{xvalidation}
\alias{xvalidation-package}
\title{xvalidation is a set of simple but effective functions to cross-validate your predictive model on your dataset.}
\usage{
  xvalidation(dataset,
    method = getOption("xvalidation.method"), k = NULL,
    names = getOption("xvalidation.foldname"),
    parallel = FALSE, pos = 1L)

  kfold(n_obs, k = getOption("xvalidation.k"),
    names = getOption("xvalidation.foldname"))

  holdout(n_obs, names = getOption("xvalidation.foldname"))

  loo(n_obs, names = getOption("xvalidation.foldname"))
}
\arguments{
  \item{dataset}{a list of elements on which will be
  performed the partitioning}

  \item{method}{the method of partitioning [default is
  \code{\link{kfold}}]}

  \item{k}{the number of folds in which partition the list
  of elements}

  \item{names}{a flag to indicate whether the folds must
  have names or not}
}
\value{
  A list of folds, each of which containing the indices of
  its train and test set
}
\description{
  xvalidation is a set of simple but effective functions to
  cross-validate your predictive model on your dataset.

  Creates the folds to perform cross-validation.
}
\section{Package options

It uses the following \code{\link{options}} to configure behaviour}{
  \itemize{ \item \code{xvalidation.k}: default number of
  folds

  \item \code{xvalidation.method}: a character vector
  indicating the implemented partitioning methods. Set it
  only to change the default behaviour of
  \code{\link{xvalidation}}, given by the first element,
  but do not use other elements that are not in this set

  \item \code{xvalidation.foldname}: control whether the
  folds have to be a named list or not

  \item \code{xvalidation.foldprefix}: specify the prefix
  to be used for the names of the folding list }
}

