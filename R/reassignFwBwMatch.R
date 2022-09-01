# The function .reassignFwBwmatch() is inspired by the function
# reassingFwBwMatch() contained in the COBRA Toolbox.
# The algorithm is the same.
.reassignFwBwMatch <- function(matchrev, keep) {
  ind               <- keep * 1
  ind[keep == TRUE] <- 1:sum(ind)

  num_match         <- sum(keep == TRUE)
  match             <- integer(num_match)
  j                 <- 0

  for (i in 1:length(matchrev)) {
      if (keep[i] == TRUE) {
          j <- j + 1
          if (matchrev[i] > 0) {
              if (keep[matchrev[i]] == TRUE) {
                  match[j] <- ind[matchrev[i]]
              }
          }
      }
  }
  return(as.integer(match))
}
