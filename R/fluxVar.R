# Performs a flux vaiability Analysis
# The function fluxVar() is inspired by the function
# fluxVariability() contained in the COBRA Toolbox.
# The algorithm is the same.
fluxVar <- function(model, react = c(1:react_num(model)), exex = FALSE, ...) {
    if (!is(model, "modelorg")) {
      stop("needs an object of class modelorg!")
    }

    # remove exchange reactions from analysis
    if (isTRUE(exex)) {
        exchReact <- findExchReact(model)
        ex <- react_pos(exchReact)
        intReact <- 1:react_num(model)
        intReact <- intReact[-ex]
        
        if (length(intReact) < 1) {
            stop("model contains no internal reactions!")
        }
    }
    else {
        intReact <- react
    }

    creact <- checkReactId(model, intReact)
    
    if (!is(creact, "reactId")) {
        stop("check argument react")
    }

#                               optimizations                                  #
    sol <- optimizer(model,
                     react = as.list(c(react_pos(creact), react_pos(creact))),
                     obj_coef = rep(1, (2*length(creact))),
                     lpdir = c(rep("min", length(creact)),
                               rep("max", length(creact))),
                     algorithm = "fv", ...)
    
#                             save the results                                 #
    optsol <- new("optsol_fluxVar")
    opt <- makeOptsolMO(model, sol)
    as(optsol, "optsol_optimizeProb") <- opt
    react(optsol) <- creact
    return(optsol)
}
