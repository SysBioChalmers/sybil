.generateWT <- function(model, react = NULL, lb = NULL, ub = NULL, ...) {
#    ca <- match.call()
#    if ("solver" %in% names(ca)) {

#        # It is necessary to test, whether argument solver has a variable,
#        # or not. If it is a variable, it needs to be evaluated.
#        testslv <- tryCatch(eval(parse(text = ca["solver"])),
#                            error = function(e) e)
#        if (is(testslv, "simpleError")) {
#            slv <- as.character(ca["solver"])
#        }
#        else {
#            slv <- as.character(testslv)
#        }
#    }
#    else {
#        slv <- SYBIL_SETTINGS("SOLVER")
#    }
	
	# setting ... parameters into list
	ca <- list(...)
	if(is.null(ca[["solver"]])){
		ca[["solver"]] <- SYBIL_SETTINGS("SOLVER")
	}

    me <- checkDefaultMethod(solver = ca[["solver"]],
                             method = "NA",
                             probType = "lp",
                             loadPackage = FALSE)
    
    ca[["solver"]] <- me[["sol"]]
    ca[["method"]] <- me[["met"]]
    ca[["solverParm"]] <- as.data.frame(NA)
    
    ca[["object"]] <- model
    if(is.null(ca[["algorithm"]])) ca[["algorithm"]] <- "fba"
    ca[["react"]] <- react
    ca[["ub"]] <- ub
    ca[["lb"]] <- lb

    if (is(react, "list")) {
        message("calculating fba solutions ... ", appendLF = FALSE)
        suppressMessages({
        	ca[["lpdir"]] <- rep("max", length(react))
        	ca[["verboseMode"]] <- 0
        	
        	tmp <- do.call(optimizer, ca)
        })
        message("OK")
    }
    else {
    	ca[["retOptSol"]] <- FALSE
    	ca[["lpdir"]] <- "max"
    	tmp <- do.call("optimizeProb", ca)
    }
    return(tmp)
}
