.ceilValues <- function(val, tol = SYBIL_SETTINGS("TOLERANCE")) {
    if (!is(val, "numeric")) {
        stop( c("Argument val has to be numeric!") )
    }
    ceilVal <- ceiling(val/tol)*tol
    return(ceilVal)
}
