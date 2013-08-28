preprocessor <- function(x) attr(x, 'preprocessor')

#' @export
'preprocessor<-' <- function(x, value) {
  assert_that(
    is.function(x),
    has_args(value, c('fold'), exact = TRUE)
  )
  attr(x, 'preprocessor') <- value
  x
}

trainer <- function(x) attr(x, 'trainer')

#' @export
'trainer<-' <- function(x, value) {
  assert_that(
    is.function(x),
    has_args(value, c('training_set'), exact = TRUE)
  )
  attr(x, 'trainer') <- value
  x
}

validator <- function(x) attr(x, 'validator')

#' @export
'validator<-' <- function(x, value) {
  assert_that(
    is.function(x),
    has_args(value, c('model', 'validation_set'), exact = TRUE)
  )
  attr(x, 'validator') <- value
  x
}

evaluator <- function(x) attr(x, 'evaluator')

#' @export
'evaluator<-' <- function(x, value) {
  assert_that(
    is.function(x),
    has_args(value, c('results', 'training_set'), exact = TRUE)
  )
  attr(x, 'evaluator') <- value
  x
}

aggregator <- function(x) attr(x, 'aggregator')

#' @export
'aggregator<-' <- function(x, value) {
  assert_that(
    is.function(x),
    has_args(value, c('stats'), exact = TRUE)
  )
  attr(x, 'aggregator') <- value
  x
}

# standardise_call <- function(call, env = parent.frame()) {
#   assert_that(is.call(call))
#   f <- eval(call[[1L]], env)
#   assert_that(!is.primitive(f))
#   match.call(f, call)
# }
