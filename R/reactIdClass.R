setClass("reactId",
         representation(
              mod_id    = "character",
              mod_key   = "character",
              react_pos = "integer",
              react_id  = "character",
              react_num = "integer"
         ),
         validity = .validreactId
)

#                            default constructor                               #
setMethod(f = "initialize",
          signature = "reactId",
          definition = function(.Object, mod_id, pnt, id = NULL, mod_key = "") {

              if ( (!missing(mod_id)) || (!missing(pnt)) ) {
              
                  nr  <- length(pnt)
                  
                  .Object@mod_id    <- as.character(mod_id)
                  .Object@mod_key   <- as.character(mod_key)
                  .Object@react_pos <- as.integer(pnt)
                  .Object@react_id  <- as.character(id)
                  .Object@react_num <- as.integer(nr)
                  validObject(.Object)
              }

              return(.Object)
          }
)


#                            setters and getters                               #
# mod_id
setMethod("mod_id", signature(object = "reactId"),
          function(object) {
              return(object@mod_id)
          }
)

setReplaceMethod("mod_id", signature(object = "reactId"),
                 function(object, value) {
                     object@mod_id <- value
                     return(object)
                 }
)

# mod_key
setMethod("mod_key", signature(object = "reactId"),
          function(object) {
              return(object@mod_key)
          }
)

setReplaceMethod("mod_key", signature(object = "reactId"),
                 function(object, value) {
                     object@mod_key <- value
                     return(object)
                 }
)

# position
setMethod("react_pos", signature(object = "reactId"),
          function(object) {
              return(object@react_pos)
          }
)

setReplaceMethod("react_pos", signature(object = "reactId"),
                 function(object, value) {
                     object@react_pos <- value
                     return(object)
                 }
)

# react_id
setMethod("react_id", signature(object = "reactId"),
          function(object) {
              return(object@react_id)
          }
)

setReplaceMethod("react_id", signature(object = "reactId"),
                 function(object, value) {
                     object@react_id <- value
                     return(object)
                 }
)

#                               other methods                                  #
setMethod("show", signature(object = "reactId"),
    function(object) {
        wcn <- trunc(log10(length(object))) + 3
        cn  <- paste("[", 1:length(object), "]", sep = "")
        cat(sprintf(paste("%", wcn, "s  ", "%-11s%s\n", sep = ""),
            "# " ,"position", "reaction id"))
        cat(sprintf(paste("%", wcn, "s  ", "%-11s%s", sep = ""),
            cn, react_pos(object), react_id(object)), sep = "\n")
        cat("\nnumber of reactions: ", length(object), "\n", sep = "")
    }
)


# length of an object of class reactId
setMethod("length", signature(x = "reactId"),
          function(x) {
              return(x@react_num)
          }
)

setMethod("[", signature(x = "reactId"),
    function(x, i, j, ..., drop = FALSE) {

        if ( (missing(i)) || (length(i) == 0) ) {
            return(x)
        }

        if (is(i, "character")) {
            ind <- match(i, react_id(x))
        }
#        else if (is(i, "reactId")) {
#            ind <- which(react_pos(x) %in% react_pos(i))
#        }
        else {
            ind <- i
        }

        if (max(ind, na.rm = TRUE) > length(x)) {
            stop("subscript out of bounds")
        }
        newRI <- new("reactId",
                     mod_id = x@mod_id,
                     pnt    = x@react_pos[ind],
                     id     = x@react_id[ind])

        return(newRI)
    }
)

