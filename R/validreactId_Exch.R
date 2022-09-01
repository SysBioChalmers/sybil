.validreactId_Exch <- function(object) {
  if (!is(object, "reactId_Exch")) {
      return("needs an object of class reactId_Exch!")
  }
  if (length(met_pos(object)) != length(object)) {
      return(paste("slot met_pos must be of length", length(object)))
  }
  if (length(met_id(object)) != length(object)) {
      return(paste("slot met_id must be of length", length(object)))
  }
  if (length(object@uptake) != length(object)) {
      return(paste("slot uptake must be of length", length(object)))
  }
  return(TRUE)
}
