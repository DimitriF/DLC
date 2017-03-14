splitdf <- function(df, n) {
  indx <- matrix(seq_len(ncol(df)), ncol = n)
  lapply(seq_len(n), function(x) df[, indx[, x]])
}

## http://stackoverflow.com/questions/29066516/r-splitting-a-data-frame-into-equal-parts-and-composing-a-list