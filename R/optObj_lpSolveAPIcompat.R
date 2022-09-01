# This is for compatibility with lpSolveAPI.
# lpSolveAPI only has a return code, which also acts as status code.

return_codeLPSOLVE <- function(code) {
    if (code == 0)       { return( "optimal solution found" ) }
    else if (code == 1)  { return( "the model is sub-optimal" ) }
    else if (code == 2)  { return( "the model is infeasible" ) }
    else if (code == 3)  { return( "the model is unbounded" ) }
    else if (code == 4)  { return( "the model is degenerate" ) }
    else if (code == 5)  { return( "numerical failure encountered" ) }
    else if (code == 6)  { return( "process aborted" ) }
    else if (code == 7)  { return( "timeout" ) }
    else if (code == 9)  { return( "the model was solved by presolve" ) }
    else if (code == 10) { return( "the branch and bound routine failed" ) }
    else if (code == 11) { return( paste("the branch and bound was stopped",
                                         "because of a break-at-first",
                                         "or break-at-value"
                                   )
                           )
    }
    else if (code == 12) { return( paste("a feasible branch and bound",
                                         "solution was found"
                                   )
                           )
    }
    else if (code == 13) { return( paste("no feasible branch and bound",
                                         "solution was found"
                                   )
                           )
    }
    else { return(paste("Failed to obtain solution",
                        "unknown error code:", code
                  )
           )
    }
}

loadMatrixPerColumnLPSOLVE <- function(lpmod, constMat) {
    stopifnot(is(constMat, "Matrix"))
    x <- constMat@x
    p <- constMat@p + 1
    i <- constMat@i + 1

    k <- 1
    while (k <= ncol(constMat)) {
        lpSolveAPI::set.column(lpmod,
                               column  = k,
                               x       = x[(p[k]):(p[k+1]-1)],
                               indices = i[(p[k]):(p[k+1]-1)])
        k <- k + 1
    }

}
