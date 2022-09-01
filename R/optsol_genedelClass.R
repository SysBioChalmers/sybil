#                  definition of the class optsol_genedel                      

setClass("optsol_genedel",
           representation(
                fluxdels  = "list",
                hasEffect = "logical"
           ),
           contains = "optsol_fluxdel"
)

#                            setters and getters                               
# reactions
setMethod("fluxdels", signature(object = "optsol_genedel"),
          function(object) {
              return(object@fluxdels)
          }
)

setReplaceMethod("fluxdels", signature(object = "optsol_genedel"),
                 function(object, value) {
                     object@fluxdels <- value
                     return(object)
                 }
)

# hasEffect
setMethod("hasEffect", signature(object = "optsol_genedel"),
          function(object) {
              return(object@hasEffect)
          }
)

setReplaceMethod("hasEffect", signature(object = "optsol_genedel"),
                 function(object, value) {
                     object@hasEffect <- value
                     return(object)
                 }
)

setMethod("deleted", signature(object = "optsol_genedel"),
                 function(object, i) {
                     value <- fluxdels(object)[[i]]
                     return(value)
                 }
)
