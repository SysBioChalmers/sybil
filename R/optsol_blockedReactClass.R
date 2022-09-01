#               definition of the class optsol_blockedReact                    #
# slot obj_function is used here for the optimized reaction
setClass("optsol_blockedReact",
         representation(
              blocked      = "logical",           # blocked reaction (yes/no)
              react        = "reactId"            # checked reaction id's
        ),
        contains = "optsol"
)

#                            setters and getters                               #
# blocked
setMethod("blocked", signature(object = "optsol_blockedReact"),
          function(object) {
              return(object@blocked)
          }
)

setReplaceMethod("blocked", signature(object = "optsol_blockedReact"),
                 function(object, value) {
                     object@blocked <- value
                     return(object)
                 }
)


# react
setMethod("react", signature(object = "optsol_blockedReact"),
          function(object) {
              return(object@react)
          }
)

setReplaceMethod("react", signature(object = "optsol_blockedReact"),
                 function(object, value) {
                     object@react <- value
                     return(object)
                 }
)

#                               other methods                                  #
setMethod("maxSol", signature(object = "optsol_blockedReact"),
          function(object, slot) {
              odds    <- seq(1, num_of_prob(object), 2)
              command <- paste(deparse(substitute(slot)), "(", deparse(substitute(object)), ")[odds]", sep = "")
              minimalSolutions <- eval(parse(text = command))
              return(minimalSolutions)
          }
)

setMethod("minSol", signature(object = "optsol_blockedReact"),
          function(object, slot) {
              odds    <- seq(2, num_of_prob(object), 2)
              command <- paste(deparse(substitute(slot)), "(", deparse(substitute(object)), ")[odds]", sep = "")
              minimalSolutions <- eval(parse(text = command))
              return(minimalSolutions)
          }
)
