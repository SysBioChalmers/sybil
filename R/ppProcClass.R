#                       definition of the class ppProc                         #
setClass("ppProc",
         representation(
              #cmd = "character",
              cmd = "list",
              pa  = "list",
              ind = "integer"
         )
)

#                              user constructor                                #
ppProc <- function(cmd) {
    if (missing(cmd)) {
        stop("Creating an object of class ppProc needs a list of commands!")
    }

    if(is(cmd, "list")) {
        cmd <- as.list(cmd)
    }

    obj <- new("ppProc", cmd = cmd)
    return(obj)
}

#                            setters and getters                               #
# cmd
setMethod("cmd", signature(object = "ppProc"),
          function(object) {
              return(object@cmd)
          }
)

setReplaceMethod("cmd", signature(object = "ppProc"),
                 function(object, value) {
                     object@cmd <- value
                     return(object)
                 }
)

# pa
setMethod("pa", signature(object = "ppProc"),
          function(object) {
              return(object@pa)
          }
)

setReplaceMethod("pa", signature(object = "ppProc"),
                 function(object, value) {
                     object@pa <- value
                     return(object)
                 }
)

# ind
setMethod("ind", signature(object = "ppProc"),
          function(object) {
              return(object@ind)
          }
)

setReplaceMethod("ind", signature(object = "ppProc"),
                 function(object, value) {
                     object@ind <- value
                     return(object)
                 }
)
