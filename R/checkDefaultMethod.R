checkDefaultMethod <- function(solver, method, probType, loadPackage = TRUE) {
    stopifnot(is(solver, "character"),
              is(method, "character"),
              is(probType, "character"))

    # validate solver
    val_solver_ind <- match(solver, .SYBILenv$solvers)

    if (is.na(val_solver_ind)) {
        cmd <- paste(solver, ".onAttach()", sep = ":::")
        slv <- tryCatch(eval(parse(text = cmd)), error = function(e) e)
        if (isTRUE(slv)) {
            val_solver_ind <- match(solver, .SYBILenv$solvers)
        }

        if (is.na(val_solver_ind)) {
            val_solver_ind <- 1L
            warning("solver ", sQuote(solver),
                    " not found, using default: ",
                    sQuote(.SYBILenv$solvers[val_solver_ind]))
        }
    }

    val_solver <- .SYBILenv$solvers[val_solver_ind]

    # validate method
    val_method_ind <- match(method, .SYBILenv$solverMethods[[val_solver]])

    if (is.na(val_method_ind)) {
        val_method <- .SYBILenv$solverMethods[[val_solver]][1]
    }
    else {
        val_method <- .SYBILenv$solverMethods[[val_solver]][val_method_ind]
    }

    # validate method with problem type
    if (probType %in% names(.SYBILenv$ptype)) {
        if (val_solver %in% names(.SYBILenv$ptype[[probType]])) {
            meth_tmp <- match(val_method, .SYBILenv$ptype[[probType]][[val_solver]])
            if (is.na(meth_tmp)) {
                val_method <- .SYBILenv$ptype[[probType]][[val_solver]][1]
            }
        }
        else {
            stop("solver ", sQuote(val_solver),
                 " can not handle problems of type ", sQuote(probType))
        }
    }

    # solver parameters
    ctrl_parm <- .SYBILenv$solverCtrlParm[[val_solver]][[val_method]]

    if (is.null(ctrl_parm)) {
        ctrl_parm <- as.data.frame(NA)
    }

    # load solver package
    if (isTRUE(loadPackage)) {
        checkPackage <- require(val_solver, character.only = TRUE)
    
        if(!isTRUE(checkPackage)) {
            stop("package ", sQuote(val_solver), " not found")
        }
    }

    return(list(sol  = as.character(val_solver),
                met  = as.character(val_method),
                parm = ctrl_parm
           )
    )
}
