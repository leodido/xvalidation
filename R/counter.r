create_counter <- function(curr_count) {
  list(
    increment = function(amount) {
      curr_count <<- curr_count + amount
    },
    value = function() {
      return(curr_count)
    }
  )
}
