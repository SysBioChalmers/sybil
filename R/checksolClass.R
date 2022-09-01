#                   definition of the class checksol                           #
setClass("checksol",
         representation(
              num_of_prob    = "integer",
              exit_code      = "integer",
              exit_num       = "integer",
              exit_meaning   = "character",
              status_code    = "integer",
              status_num     = "integer",
              status_meaning = "character"
        )
)

#                              user constructor                                #
checksol <- function() {
    new("checksol")
}

#                            setters and getters                               #
# num_of_prob
setMethod("num_of_prob", signature(object = "checksol"),
          function(object) {
              return(object@num_of_prob)
          }
)

setReplaceMethod("num_of_prob", signature(object = "checksol"),
                 function(object, value) {
                     object@num_of_prob <- value
                     return(object)
                 }
)

# exit_code
setMethod("exit_code", signature(object = "checksol"),
          function(object) {
              return(object@exit_code)
          }
)

setReplaceMethod("exit_code", signature(object = "checksol"),
                 function(object, value) {
                     object@exit_code <- value
                     return(object)
                 }
)

# exit_num
setMethod("exit_num", signature(object = "checksol"),
          function(object) {
              return(object@exit_num)
          }
)

setReplaceMethod("exit_num", signature(object = "checksol"),
                 function(object, value) {
                     object@exit_num <- value
                     return(object)
                 }
)


# exit_meaning
setMethod("exit_meaning", signature(object = "checksol"),
          function(object) {
              return(object@exit_meaning)
          }
)

setReplaceMethod("exit_meaning", signature(object = "checksol"),
                 function(object, value) {
                     object@exit_meaning <- value
                     return(object)
                 }
)


# status_code
setMethod("status_code", signature(object = "checksol"),
          function(object) {
              return(object@status_code)
          }
)

setReplaceMethod("status_code", signature(object = "checksol"),
                 function(object, value) {
                     object@status_code <- value
                     return(object)
                 }
)


# status_num
setMethod("status_num", signature(object = "checksol"),
          function(object) {
              return(object@status_num)
          }
)

setReplaceMethod("status_num", signature(object = "checksol"),
                 function(object, value) {
                     object@status_num <- value
                     return(object)
                 }
)



# status_meaning
setMethod("status_meaning", signature(object = "checksol"),
          function(object) {
              return(object@status_meaning)
          }
)

setReplaceMethod("status_meaning", signature(object = "checksol"),
                 function(object, value) {
                     object@status_meaning <- value
                     return(object)
                 }
)


#                               other methods                                  #
setMethod("show", signature(object = "checksol"),
    function(object) {
        cat("Return code:\n")
        cat(" Code    #       meaning\n")
        tmp <- sprintf(" %-8i%-8i%s\n",
                       exit_code(object),
                       exit_num(object),
                       exit_meaning(object))
        cat(tmp, sep = "")
        cat("\n")
        cat("Solution status:\n")
        cat(" Code    #       meaning\n")
        tmp <- sprintf(" %-8i%-8i%s\n",
                       status_code(object),
                       status_num(object),
                       status_meaning(object))
        cat(tmp, sep = "")
        num <- num_of_prob(object)
        if (num > 1) {
            cat("\n", num, " optimizations were performed.\n", sep = "")
        }
    }
)
