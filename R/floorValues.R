.floorValues <- function(val, tol = SYBIL_SETTINGS("TOLERANCE")) {
    if (!is(val, "numeric")) {
        stop( c("Argument val has to be numeric!") )
    }
    floorVal <- floor(val/tol)*tol
    return(floorVal)
}
