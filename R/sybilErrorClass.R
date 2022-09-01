#                     definition of the class sybilError                       
setClass("sybilError",
         representation(
              emsg = "character",
              enum = "integer"
         )
)

#                              user constructor                                
sybilError <- function(errmsg = "", number = NA) {

    msg <- paste(errmsg, collapse = " ")

    obj <- new(Class = "sybilError",
               emsg = as.character(msg),
               enum = as.integer(number)
    )

    return(obj)
}

#                            default constructor                               
setMethod(f = "initialize",
          signature = "sybilError",
          definition = function(.Object, emsg, enum) {

    if (missing(emsg)) {
        stop("creation of instances of class sybilError needs a message")
    }
    else {}

    if (missing(enum)) {
        enum <- NA
    }
    else {}

    .Object@emsg = as.character(emsg)
    .Object@enum = as.integer(enum)

    validObject(.Object)
    return(.Object)
}
)

#                            setters and getters                               
# msg
setMethod(f = "emsg",
          signature = "sybilError",
          definition = function(object) {
              return(object@emsg)
          }
)

setReplaceMethod(f = "emsg",
                 signature = "sybilError",
                 definition = function(object, value) {
                     object@emsg <- value
                     return(object)
                 }
)

# num
setMethod(f = "enum",
          signature = "sybilError",
          definition = function(object) {
              return(object@enum)
          }
)

setReplaceMethod(f = "enum",
                 signature = "sybilError",
                 definition = function(object, value) {
                     object@enum <- value
                     return(object)
                 }
)

#                               other methods                                  
setMethod("show", signature(object = "sybilError"),
    function(object) {
        cat("error no.:", enum(object), "\n")
        cat(emsg(object), "\n")
    }
)

