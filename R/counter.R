counter <- function() {
  starting_val <- 0
  counter_fn <- function() {
    starting_val <<- starting_val + 1
  }
}
