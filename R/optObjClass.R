#                       definition of the class optObj                         
setClass(Class = "optObj",
         representation(
              oobj     = "pointerToProb",
              solver   = "character",
              method   = "character",
              probType = "character"
         ),
         contains = "VIRTUAL"
)

# derivatives
#setClass(Class = "optObj_boot", contains = "optObj")

#                              user constructor                                
optObj <- function(solver = SYBIL_SETTINGS("SOLVER"),
                   method = SYBIL_SETTINGS("METHOD"),
                   pType = "lp", prefix = "optObj", sep = "_") {

    validSoMe <- checkDefaultMethod(solver, method, pType)

    obj <- new(paste(prefix, validSoMe$sol, sep = sep),
               sv = validSoMe$sol,
               mt = validSoMe$met,
               pt = as.character(pType))

    return(obj)
}

#                            default constructor                               
# contructor for class optObj
setMethod(f = "initialize",
          signature = "optObj",
          definition = function(.Object, sv, mt, pt) {

              if ( (!missing(sv)) && (!missing(mt)) && (!missing(pt)) ) {
                  
                  .Object@solver   <- as.character(sv)
                  .Object@method   <- as.character(mt)
                  .Object@probType <- as.character(pt)
                  
              }
              return(.Object)
          }
)

#                                  getters                                     
# solver
setMethod("solver", signature(object = "optObj"),
          function(object) {
              return(object@solver)
          }
)


# method
setMethod("method", signature(object = "optObj"),
          function(object) {
              return(object@method)
          }
)


# probType
setMethod("probType", signature(object = "optObj"),
          function(object) {
              return(object@probType)
          }
)

#                               other methods                                  
# get the current dimension of the constraint matrix
setMethod("dim", "optObj",

    function(x) {

        out <- c(0, 0)
        out[1] <- getNumRows(x)
        out[2] <- getNumCols(x)

        return(out)
    }
)

setMethod("show", signature(object = "optObj"),
    function(object) {
        if (length(probType(object)) > 0) {
            switch (probType(object),
                "lp" = {
                    cat("linear programming problem object\n")
                },
                "mip" = {
                    cat("mixed integer linear programming problem object\n")
                },
                "qp" = {
                    cat("continuous problem object with quadratic objective\n")
                },
                {
                    cat("problem object of type ", probType(object),"\n")
                }
            )
            cat("solver:", solver(object), "\n")
            cat("method:", method(object), "\n")
            size <- tryCatch(dim(object), error = function(e) NA)
            if (any(is.na(size))) {
                cat("problem is not initialized\n")
            }
            else if (all(size == 0)) {
                cat("problem is currently empty\n")
            }
            else {
                cat("problem has", size[2],
                    ngettext(size[2], "variable", "variables"))
                cat(" and", size[1],
                    ngettext(size[1], "constraint", "constraints"), "\n")
            }
        }
        else {
            cat("empty problem object\n")
        }
    }
)
