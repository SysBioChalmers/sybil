
#                  definition of the class optsol_fluxdel                      

setClass("optsol_fluxdel",
           representation(
               chlb      = "numeric",      # lower bound of changed fluxes/genes
               chub      = "numeric",      # upper bound of changed fluxes/genes
               dels      = "matrix"        # id's of deleted fluxes
           ),
           contains = "optsol_optimizeProb"
)

#                            setters and getters                               

# chlb
setMethod("chlb", signature(object = "optsol_fluxdel"),
          function(object) {
              return(object@chlb)
          }
)

setReplaceMethod("chlb", signature(object = "optsol_fluxdel"),
                 function(object, value) {
                     object@chlb <- value
                     return(object)
                 }
)


# chub
setMethod("chub", signature(object = "optsol_fluxdel"),
          function(object) {
              return(object@chub)
          }
)

setReplaceMethod("chub", signature(object = "optsol_fluxdel"),
                 function(object, value) {
                     object@chub <- value
                     return(object)
                 }
)


# dels
setMethod("dels", signature(object = "optsol_fluxdel"),
          function(object) {
              return(object@dels)
          }
)

setReplaceMethod("dels", signature(object = "optsol_fluxdel"),
                 function(object, value) {
                     object@dels <- value
                     return(object)
                 }
)

#                               other methods                                  

# lethal
setMethod("lethal", signature(object = "optsol_fluxdel"),
          function(object, wt, tol) {
              
              stopifnot(is(wt, "numeric"), length(wt) == 1)

              if (missing(tol)) {
                  tol <- SYBIL_SETTINGS("TOLERANCE")
              }
              
              letid <- which(abs(mod_obj(object)/wt) < tol)
              let   <- logical(num_of_prob(object))
              let[letid] <- TRUE
              
              return(let)
          }
)


setMethod("deleted", signature(object = "optsol_fluxdel"),
                 function(object, i) {
                     value <- dels(object)[i, ]
                     return(value)
                 }
)

setMethod("[", "optsol_fluxdel", function(x, i, j, ..., drop = FALSE) {

        if ((missing(i)) || (length(i) == 0)) {
            return(x)
        }

        if (max(i) > length(x)) {
            stop("subscript out of bounds")
        }

        slots <- slotNames(x)
        
        isO <- is(x)[1]
        
        newSol <- new(isO,
            mod_id      = x@mod_id,
            mod_key     = x@mod_key,
            solver      = x@solver,
            method      = x@method,
            algorithm   = x@algorithm,
            num_of_prob = length(i),
            lp_num_cols = x@lp_num_cols,
            lp_num_rows = x@lp_num_rows,
            lp_obj      = x@lp_obj[i],
            lp_ok       = x@lp_ok[i],
            lp_stat     = x@lp_stat[i],
            lp_dir      = x@lp_dir,
            obj_coef    = x@obj_coef,
            obj_func    = x@obj_func,
            fldind      = x@fldind,
            chlb        = x@chlb[i],
            chub        = x@chub[i],
            dels        = x@dels[i, , drop = FALSE]
        )

        if (nfluxes(x) > 1) {
            NC_fl <- TRUE
        }
        else {
            NC_fl <- FALSE
        }

        if (isTRUE(NC_fl)) {
            newSol@fluxdist <- fluxDistribution(x@fluxes[ , i, drop = FALSE])
        }

        if ("fluxdels" %in% slots) {
            newSolfluxdels <- x@fluxdels[i]
        }

        if ("hasEffect" %in% slots) {
            newSol@hasEffect <- x@hasEffect[i]
        }

        return(newSol)

    }
)


