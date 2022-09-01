# This function finds blocked reactions in a specified IO-environment.
blockedReact <- function(model,
                         tol = SYBIL_SETTINGS("TOLERANCE"),
                         exex = TRUE,
                         fld = FALSE,
                         retOptSol = FALSE,
                         verboseMode = 2,
                         ...
                        ) {

    .Deprecated(new = "fluxVar")

    if (!is(model, "modelorg")) {
        stop("needs an object of class modelorg!")
    }

    modIrr <- ifelse(is(model, "modelorg_irrev"), TRUE, FALSE) 

    intReact <- NA

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
        intReact <- 1:react_num(model)
    }

    # memory for results
    nObj <- ifelse(isTRUE(modIrr), length(intReact), 2 * length(intReact)) 
    blocked_react <- logical(react_num(model))

    if (isTRUE(retOptSol)) {
        obj   <- numeric(nObj)
        ok    <- integer(nObj)
        stat  <- integer(nObj)
        if (isTRUE(fld)) {
            flux <- Matrix::Matrix(0, nrow = react_num(model), ncol = nObj)
        }
        else {
            flux <- NA
        }
    }
    
    obj_mod <- obj_coef(model)
    obj_coef(model) <- as.integer(rep(0, react_num(model)))

	lpmod <- sysBiolAlg(model, algorithm = "fv", tol = tol, ...)

#                        finding blocked reactions                             #
    if (verboseMode > 0) { message("calculating blocked reactions ...") }

    obj_max <- 0
    obj_min <- 0
    
    if (verboseMode > 1) { progr <- .progressBar() }
  
    for (i in seq(along = intReact)) {

        solpl <- ifelse(isTRUE(modIrr), i+1, 2*i)
        
#         if (uppbnd(model)[intReact[i]] > 0) {
        
            #print(intReact[i])
            sol <- optimizeProb(lpmod, lpdir = "max",
                                react = intReact[i], obj_coef = 1)
            obj_max <- sol$obj
            if (isTRUE(retOptSol)) {
                obj[(solpl-1)]   <- sol$obj
                ok[(solpl-1)]    <- sol$ok
                stat[(solpl-1)]  <- sol$stat
                if (isTRUE(fld)) {
                    flux[,(solpl-1)] <- sol$fluxes
                }
            }
        if (!is(model, "modelorg_irrev")) {
        
            sol <- optimizeProb(lpmod, lpdir = "min",
                                react = intReact[i], obj_coef = 1)
            obj_min <- sol$obj
            if (isTRUE(retOptSol)) {
                obj[solpl]   <- sol$obj
                ok[solpl]    <- sol$ok
                stat[solpl]  <- sol$stat
                if (isTRUE(fld)) {
                    flux[,solpl] <- sol$fluxes
                }
            }
        }

        if (is(model, "modelorg_irrev")) {
            blocked_react[intReact[i]] <- ifelse(abs(obj_max) < tol, TRUE, FALSE)
        }
        else {
            if ( (abs(obj_max) < tol) && (abs(obj_min) < tol) ) {
                blocked_react[intReact[i]] <- TRUE
            }
            else {
                blocked_react[intReact[i]] <- FALSE
            }
        }

        if (verboseMode > 1) {
            progr <- .progressBar(i, length(intReact), progr)
        }

    }

    if (isTRUE(retOptSol)) {
        optsol <- new("optsol_blockedReact",
            mod_id       = mod_id(model),
            mod_key      = mod_key(model),
            solver       = solver(problem(lpmod)),
            method       = method(problem(lpmod)),
            algorithm    = algorithm(lpmod),
            num_of_prob  = as.integer(nObj),
            lp_num_cols  = nc(lpmod),
            lp_num_rows  = nr(lpmod),
            lp_obj       = as.numeric(obj),
            lp_ok        = as.integer(ok),
            lp_stat      = as.integer(stat),
            lp_dir       = factor(rep(c("max", "min"), length(intReact))),
            obj_coef     = obj_mod,
            fldind       = fldind(lpmod),
            fluxdist     = fluxDistribution(flux),
    
            blocked      = blocked_react,
            react        = new("reactId",
                               mod_id  = mod_id(model),
                               mod_key = mod_key(model),
                               pnt     = intReact,
                               id      = react_id(model)[intReact])
        )
    }
    else {
        optsol <- blocked_react
    }
    return(optsol)
}
