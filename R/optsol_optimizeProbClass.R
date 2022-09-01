
#               definition of the class optsol_optimizeProb                    

setClass("optsol_optimizeProb",
         representation(
              preProc  = "ppProc", # preprocessing lp result
              postProc = "ppProc"  # postprocessing lp result
        ),
        contains = "optsol"
)

#                              user constructor                                
makeOptsolMO <- function(mod, sol) {

    # mod is of class modelorg
    # sol is the return value of optimizer()
    
    stopifnot(is(mod, "modelorg"), is(sol, "list"))

    opt <- new("optsol_optimizeProb",
        mod_id       = mod_id(mod),
        mod_key      = mod_key(mod),
        solver       = sol[["solver"]],
        method       = sol[["method"]],
        algorithm    = sol[["algorithm"]],
        num_of_prob  = as.integer(length(sol[["obj"]])),
        lp_num_cols  = as.integer(sol[["lp_num_cols"]]),
        lp_num_rows  = as.integer(sol[["lp_num_rows"]]),
        lp_obj       = as.numeric(sol[["obj"]]),
        lp_ok        = as.integer(sol[["ok"]]),
        lp_stat      = as.integer(sol[["stat"]]),
        lp_dir       = sol[["lp_dir"]],
        obj_coef     = obj_coef(mod),
        obj_func     = printObjFunc(mod),
        fldind       = as.integer(sol[["fldind"]]),
        fluxdist     = sol[["fluxdist"]],
        alg_par      = sol[["alg_par"]])


        if (!is.null(sol$prAna)) {
            preProc(opt) <- sol[["prAna"]]
        }

        if (!is.null(sol$poAna)) {
            postProc(opt) <- sol[["poAna"]]
        }

    return(opt)
}

#                            setters and getters                               
# preProc
setMethod("preProc", signature(object = "optsol_optimizeProb"),
          function(object) {
              return(object@preProc)
          }
)

setReplaceMethod("preProc", signature(object = "optsol_optimizeProb"),
                 function(object, value) {
                     object@preProc <- value
                     return(object)
                 }
)

# postProc
setMethod("postProc", signature(object = "optsol_optimizeProb"),
          function(object) {
              return(object@postProc)
          }
)

setReplaceMethod("postProc", signature(object = "optsol_optimizeProb"),
                 function(object, value) {
                     object@postProc <- value
                     return(object)
                 }
)
