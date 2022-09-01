setClass("modelorg_irrev",
         representation(
                irrev     = "logical",
                matchrev  = "integer",
                rev2irrev = "matrix",
                irrev2rev = "integer"
         ),
         contains = "modelorg"
)

modelorg_irrev <- function(id, name) {
    if (missing(id) || missing(name)) {
        stop("Creating an object of class modelorg_irrev needs name and id!")
    }
    id   <- as.character(id)
    name <- as.character(name)
    obj <- new("modelorg_irrev", id = id, name = name)
    return(obj)
}

setMethod(f = "initialize",
          signature = "modelorg_irrev",
          definition = function(.Object, id, name) {

              if (!missing(id) || !missing(name)) {
                  .Object <- callNextMethod(.Object, id = id, name = name)
              }
              
              return(.Object)
          }
)

setMethod("irrev", signature(object = "modelorg_irrev"),
          function(object) {
              return(object@irrev)
          }
)

setReplaceMethod("irrev", signature(object = "modelorg_irrev"),
                 function(object, value) {
                     object@irrev <- value
                     return(object)
                 }
)


setMethod("matchrev", signature(object = "modelorg_irrev"),
          function(object) {
              return(object@matchrev)
          }
)

setReplaceMethod("matchrev", signature(object = "modelorg_irrev"),
                 function(object, value) {
                     object@matchrev <- value
                     return(object)
                 }
)


setMethod("rev2irrev", signature(object = "modelorg_irrev"),
         function(object) {
             return(object@rev2irrev)
         }
)

setReplaceMethod("rev2irrev", signature(object = "modelorg_irrev"),
                function(object, value) {
                    object@rev2irrev <- value
                    return(object)
                }
)

setMethod("irrev2rev", signature(object = "modelorg_irrev"),
         function(object) {
             return(object@irrev2rev)
         }
)

setReplaceMethod("irrev2rev", signature(object = "modelorg_irrev"),
                function(object, value) {
                    object@irrev2rev<- value
                    return(object)
                }
)
