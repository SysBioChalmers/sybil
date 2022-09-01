changeBounds <- function(model, react, lb = NULL, ub = NULL) {
    if (!is(model, "modelorg")) {
        stop("needs an object of class modelorg!")
    }
  
    checkedIds <- checkReactId(model, react)
    if (!is(checkedIds, "reactId")) {
        stop("argument react is wrong")
    }

    if ( (is.null(lb)) && (is.null(ub)) ) {
        lowbnd(model)[react_pos(checkedIds)] <- rep(0, length(checkedIds))
        uppbnd(model)[react_pos(checkedIds)] <- rep(0, length(checkedIds))
    }
    else {
        # set upper bound
        if (!is.null(ub)) {
            stopifnot(is(ub, "numeric"))
            if (length(ub) == 1) {
                ubnd <- rep(ub, length(checkedIds))
            }
            else {
                stopifnot(length(ub) == length(checkedIds))
                ubnd <- ub
            }
            uppbnd(model)[react_pos(checkedIds)] <- ubnd
        }

        if (!is.null(lb)) {
            stopifnot(is(lb, "numeric"))
            if (length(lb) == 1) {
                lbnd <- rep(lb, length(checkedIds))
            }
            else {
                stopifnot(length(lb) == length(checkedIds))
                lbnd <- lb
            }
            lowbnd(model)[react_pos(checkedIds)] <- lbnd
        }
    }
    return(model)
}
