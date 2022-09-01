# The function addExchReact() is inspired by the function
# addExchangeRxn() contained in the COBRA Toolbox.
# The algorithm is (more or less) the same.
addExchReact <- function(model, met, lb, ub) {
    # check arguments
    if (!is(model, "modelorg")) {
        stop("needs an object of class modelorg")
    }

    if ( (length(met) < 1) || (any(met == "")) ) {
        stop("at least one metabolite is required and all metabolite character strings required to be non-empty")
    }

    if (missing(lb)) {
        Clb <- rep(0, length(met))
    }
    else {
        Clb <- lb
    }

    if (missing(ub)) {
        Cub <- rep(SYBIL_SETTINGS("MAXIMUM"), length(met))
    }
    else {
        Cub <- ub
    }

    if ( (length(met) != length(Clb)) || (length(met) != length(Cub)) ) {
        stop("arguments 'met', 'lb' and 'ub' must have the same length")
    }

    Crev <- rep(FALSE, length(met))
    Crev[Clb < 0] <- TRUE
    
    exRid <- paste("Ex_", met, sep = "")
    
    mod_out <- model
    
    for (i in seq(along = met)) {
        mod_out <- addReact(model = mod_out,
                            id = exRid[i],
                            met = met[i],
                            Scoef = -1,
                            reversible = Crev[i],
                            lb = Clb[i],
                            ub = Cub[i])
    }

    return(mod_out)
}
