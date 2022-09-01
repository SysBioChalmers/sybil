wrong_type_msg <- function(lp) {
    warning(paste("Slot oobj of", lp@solver,
                  "is not a valid pointer to a valid solver."
            )
    )
}

wrong_solver_msg <- function(lp, method, printOut = TRUE) {
    if (isTRUE(printOut)) {
        warning(paste("Requested method", method,
                      "is not available for solver", lp@solver
                )
        )
    }
}

# which optimizations did not end successful
checkSolStat <- function(stat, solver = SYBIL_SETTINGS("SOLVER")) {

    out <- FALSE
    switch(solver,
        "glpkAPI" = {
            out <- which(stat != 5)
        },
        "clpAPI" = {
            out <- which(stat != 0)
        },
        "lpSolveAPI" = {
            out <- which(stat != 0)
        },
        "cplexAPI" = {
            # CPLEX: 101 optimal integer solution
            # CPLEX: 102 Optimal solution with the tolerance defined by epgap or epagap
            out <- which(stat != 1 & stat != 101 & stat != 102)
        },
        {
            cmd <- paste(solver,"::checkSolutionStatus(stat)", sep = "")
            out <- tryCatch(eval(parse(text = cmd)), error = function(e) NA)    
            #warning("not a valid solver")
        }
    )
    return(out)
}

getMeanReturn <- function(code, solver = SYBIL_SETTINGS("SOLVER")) {
    out <- FALSE
    switch(solver,
        "glpkAPI" = {
            out <- glpkAPI::return_codeGLPK(code)
        },
        "clpAPI" = {
            out <- clpAPI::return_codeCLP(code)
        },
        "lpSolveAPI" = {
            out <- return_codeLPSOLVE(code)
        },
        "cplexAPI" = {
            out <- cplexAPI::return_codeCPLEX(code)
        },
        {
            cmd <- paste(solver,"::getReturnString(code)", sep = "")
            out <- tryCatch(eval(parse(text = cmd)),
                            error = function(e) as.character(NA))    
            #warning("not a valid solver")
        }
    )
    return(out)
}

getMeanStatus <- function(code,
                          solver = SYBIL_SETTINGS("SOLVER"), env = NULL) {
    out <- FALSE
    switch(solver,
        "glpkAPI" = {
            out <- glpkAPI::status_codeGLPK(code)
        },
        "clpAPI" = {
            out <- clpAPI::status_codeCLP(code)
        },
        "lpSolveAPI" = {
            #out <- "see return code"
            out <- getMeanReturn(code, solver)
        },
        "cplexAPI" = {
            if (is.null(env)) {
                cenv <- cplexAPI::openEnvCPLEX()
            }
            else {
                cenv <- env
            }
            out <- cplexAPI::status_codeCPLEX(cenv, code)
            if (is.null(env)) {
                cplexAPI::closeEnvCPLEX(cenv)
            }
            rm(cenv)
        },
        {
            cmd <- paste(solver,"::getStatusString(code)", sep = "")
            out <- tryCatch(eval(parse(text = cmd)),
                            error = function(e) as.character(NA))    
            #warning("not a valid solver")
        }
    )
    return(out)
}
