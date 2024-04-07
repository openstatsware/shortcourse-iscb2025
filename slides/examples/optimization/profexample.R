# Example code
f <- function() {
  pause(0.1)
  for (i in seq_len(3)) {
    g()
  }
}
g <- function() {
  pause(0.1)
}
