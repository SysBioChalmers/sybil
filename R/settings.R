SYBIL_SETTINGS <- function(parm, value, ...) {
    if ( (missing(parm)) && (missing(value)) ) {
       return(.SYBILenv$settings)
    }

    if (missing(value)) {
        if (!parm %in% names(.SYBILenv$settings)) {
            stop("unknown parameter ", sQuote(parm))
        }
        return(.SYBILenv$settings[[parm]])
    }
    
    if ( (length(parm) != 1) ||
         ( (length(value) != 1) && (! (parm == "SOLVER_CTRL_PARM") ) ) ) {
        stop("arguments 'parm' and 'value' must have a length of 1")
    }
    
    switch(parm,
        "MODELORG_VERSION" = {
        	stop("this value must not be set by the user!")
        },
    
        "SOLVER" = {

            chmet <- checkDefaultMethod(solver = value,
                                        method = "",
                                        probType = "", ...)
            .SYBILenv$settings[["SOLVER"]]           <- chmet$sol
            .SYBILenv$settings[["METHOD"]]           <- chmet$met
            .SYBILenv$settings[["SOLVER_CTRL_PARM"]] <- chmet$parm
        },
    
        "METHOD" = {
            chmet <- checkDefaultMethod(solver = SYBIL_SETTINGS("SOLVER"),
                                        method = value,
                                        probType = "", ...)
            .SYBILenv$settings[["SOLVER"]]           <- chmet$sol
            .SYBILenv$settings[["METHOD"]]           <- chmet$met
            .SYBILenv$settings[["SOLVER_CTRL_PARM"]] <- chmet$parm
        },
    
        "TOLERANCE" = {
            .SYBILenv$settings[["TOLERANCE"]] <- as.numeric(value)
        },
    
        "MAXIMUM" = {
            .SYBILenv$settings[["MAXIMUM"]] <- as.numeric(value)
        },
    
        "ALGORITHM" = {
#            if ( (value == "FBA")            ||
#                 (value == "linearMOMA")     ||
#                 (value == "linearMOMA_COBRA") ) {
#                .SYBILenv$settings[["ALGORITHM"]] <- as.character(value)
#            }
#            else {
#                stop("ALGORITHM can be either 'FBA', ",
#                     "'linearMOMA' or 'linearMOMA_COBRA'")
#            }
            .SYBILenv$settings[["ALGORITHM"]] <- as.character(value)
        },
    
        "OPT_DIRECTION" = {
            if ( (value == "max") || (value == "min") ) {
                .SYBILenv$settings[["OPT_DIRECTION"]] <- as.character(value)
            }
            else {
                stop("OPT_DIRECTION can be either 'max' or 'min'")
            }
        },
    
        "USE_NAMES" = {
            .SYBILenv$settings[["USE_NAMES"]] <- as.logical(value)
        },

        "PATH_TO_MODEL" = {
            if (file.exists(value)) {
                .SYBILenv$settings[["PATH_TO_MODEL"]] <- as.character(value)
            }
            else {
                stop("directory ", sQuote(value), " does not exist")
            }
        },
    
        "SOLVER_CTRL_PARM" = {
            if ( (is.data.frame(value)) || (is.list(value)) ) {
                if ("NA" %in% names(SYBIL_SETTINGS("SOLVER_CTRL_PARM"))) {
                    .SYBILenv$settings[["SOLVER_CTRL_PARM"]] <- value
                }
                else {
                    pn <- names(value)
                    for (i in seq(along = value)) {
                        .SYBILenv$settings[["SOLVER_CTRL_PARM"]][[pn[i]]] <- value[[pn[i]]]
                    }
                }
            }
            else {
                stop("SOLVER_CTRL_PARM must be data.frame or list")
            }
        },

        {
            stop("unknown parameter: ", sQuote(parm))
        }
    )
}
