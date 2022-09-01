#                  definition of the class optsol_robAna                       
setClass("optsol_robAna",
         representation(
              ctrlr   = "reactId",   # id of the control reaction,
              ctrlfl  = "numeric"     # fixed flux values of control reaction
         ),
         contains = "optsol_optimizeProb"
)

#                            setters and getters                               
# ctrlr
setMethod("ctrlr", signature(object = "optsol_robAna"),
          function(object) {
              return(object@ctrlr)
          }
)

setReplaceMethod("ctrlr", signature(object = "optsol_robAna"),
                 function(object, value) {
                     object@ctrlr <- value
                     return(object)
                 }
)

# ctrlfl
setMethod("ctrlfl", signature(object = "optsol_robAna"),
          function(object) {
              return(object@ctrlfl)
          }
)

setReplaceMethod("ctrlfl", signature(object = "optsol_robAna"),
                 function(object, value) {
                     object@ctrlfl <- value
                     return(object)
                 }
)

#                               other methods                                  
setMethod("plot", signature(x = "optsol_robAna", y = "missing"),
          function(x, y,
                   xlab = paste("Control Flux:", react_id(ctrlr(x))),
                   ylab = paste("Objective Function:", obj_func(x)),
                   type = "b",
                   pch = 20,
                   fillColorBg = "grey",
                   fillBg = TRUE,
                   absCtrl = TRUE,
                   ...) {

              if (isTRUE(absCtrl)) {
                  cr <- abs(x@ctrlfl)
              }
              else {
                  cr <- x@ctrlfl
              }

              plot(cr, x@lp_obj, type = "n", xlab = xlab, ylab = ylab)
              if (isTRUE(fillBg)) {
                  polygon(c(cr[1], cr, cr[length(cr)]),
                          c(min(x@lp_obj), x@lp_obj, min(x@lp_obj)),
                          col = fillColorBg, border = NA)
              }
              points(cr, x@lp_obj, type = type, pch = pch, ...)
          }
)
