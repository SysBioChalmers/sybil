changeObjFunc <- function(model, react, obj_coef = rep(1, length(react))) {
  if (!is(model, "modelorg")) {
      stop("needs an object of class modelorg!")
  }

  if (length(react) != length(obj_coef)) {
      stop("react and obj_coef must have the same length!")
  }

  checkedIds <- checkReactId(model, react)
  if (!is(checkedIds, "reactId")) {
      stop("Check your reaction Id's")
  }
 
  # set all objective coefficients to zero
  obj_coef(model) <- numeric(react_num(model))
  obj_coef(model)[react_pos(checkedIds)] <- obj_coef
  return(model)
}
