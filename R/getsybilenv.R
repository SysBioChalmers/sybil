getsybilenv <- function(part) {
    printsolvers <- function() {
        cat("\n# --------------------------------------------------------- #\n")
        cat("solver packages:\n")
        cat(paste(.SYBILenv[["solvers"]]), sep = ", ")
        cat("\n\n")
    }
    printmethods <- function() {
        cat("\n# --------------------------------------------------------- #\n")
        cat("methods included in the solver packages:\n")
        slv <- names(.SYBILenv[["solverMethods"]])
        for(i in seq(along = slv)) {
            cat(slv[i], ":\n", sep = "")
            cat(paste(.SYBILenv[["solverMethods"]][[slv[i]]]), sep = ", ")
            cat("\n\n")
        }
        cat("\n")
    }
    printptype <- function() {
        cat("\n# --------------------------------------------------------- #\n")
        cat("methods used for problem types:\n")
        ptype <- names(.SYBILenv[["ptype"]])
        for(i in seq(along = ptype)) {
            cat(ptype[i], ":\n", sep = "")
            slv <- names(.SYBILenv[["ptype"]][[ptype[i]]])
            for(j in seq(along = slv)) {
                cat("    ", slv[j], ":\n    ", sep = "")
                cat(paste(.SYBILenv[["ptype"]][[ptype[i]]][[slv[j]]]), sep = ", ")
                cat("\n\n")
            }
        }
        cat("\n")
    }
    printpurpose <- function() {
        cat("\n# --------------------------------------------------------- #\n")
        cat("algorithms for this purpose:\n")
        fkt <- names(.SYBILenv[["algorithm"]])
        for(i in seq(along = fkt)) {
            cat(fkt[i], ":\n", sep = "")
            cat(paste(.SYBILenv[["algorithm"]][[fkt[i]]]), sep = ", ")
            cat("\n\n")
        }
        cat("\n")
    }
    if (missing(part)) {
        printsolvers()
        printmethods()
        printptype()
        printpurpose()
        #print(.SYBILenv[["solverCtrlParm"]])
    }
    else {
        cmd <- paste("print", part, "()", sep = "")
        err <- tryCatch(eval(parse(text = cmd)), error = function(e) e)
        if (is(err, "simpleError")) {
            stop(sQuote(part), " is not available in the sybil environment")
        }
    }
    return(invisible(NULL))
}
