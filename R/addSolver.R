addSolver <- function(solver, method, probType) {
  
    stopifnot(inherits(solver, "character"),
              inherits(method, "character"),
              inherits(probType, "list"),
              (length(solver) == 1L),
              length(probType) == length(method))

    if (solver %in% .SYBILenv$solvers) {
        return(invisible(FALSE))
    }
    else {
        .SYBILenv$solvers <- append(.SYBILenv$solvers, solver)
    }

    .SYBILenv$solverMethods[[solver]] <- method
    .SYBILenv$solverCtrlParm[[solver]] <- vector(length = length(method),
                                                 mode = "list")

    names(.SYBILenv$solverCtrlParm[[solver]]) <- method

    for (i in seq(along = method)) {
        .SYBILenv$solverCtrlParm[[solver]][[i]] <- as.data.frame(NA)
    }

    for (i in seq(along = method)) {
        pt <- unique(probType[[i]])
        stopifnot(inherits(pt, "character"))
        prtp <- pt %in% names(.SYBILenv$ptype)
        for (j in seq(along = pt[prtp])) {
            .SYBILenv$ptype[[pt[prtp][j]]][[solver]] <- append(.SYBILenv$ptype[[pt[prtp][j]]][[solver]], method[i])
        }
    }

    return(invisible(TRUE))
}
