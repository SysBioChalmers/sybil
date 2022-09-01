editEnvir <- function(model, newKey = FALSE, ...) {
    if (!is(model, "modelorg")) {
        stop("model must be of class modelorg")
    }
    ex <- findExchReact(model)
    exid <- react_pos(ex)

    exfr <- data.frame("reaction_id"   = react_id(model)[exid],
                       "lower_bound"   = lowbnd(model)[exid],
                       "upper_bound"   = uppbnd(model)[exid],
                       "reaction_name" = react_name(model)[exid])

    exfr <- edit(exfr, ...)

    lowbnd(model)[exid] <- exfr[["lower_bound"]]
    uppbnd(model)[exid] <- exfr[["upper_bound"]]
    
    if (isTRUE(newKey)) {
        mod_key(model) <- .generateModKey()
    }
    validObject(model)
    return(model)
}
