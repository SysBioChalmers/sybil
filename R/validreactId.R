# Validity checking of an object of class reactId
.validreactId <- function(object) {
  if (!is(object, "reactId")) {
      return("needs an object of class reactId!")
  }
  if (length(mod_id(object)) != 1) {
      return("slot mod_id must be of length 1")
  }
  if (length(react_pos(object)) != length(object)) {
      return(paste("slot react_pos must be of length", length(object)))
  }
  if (length(react_id(object)) != length(object)) {
      return(paste("slot react_id must be of length", length(object)))
  }
  return(TRUE)
}
