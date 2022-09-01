#                            class definitions                                 #
setClass("netFlux",
    representation(
        uptake       = "logical",
        product      = "logical",
        unused       = "logical",
        react_id     = "character",
        rate         = "numeric"
    ),
)

#                              user constructor                                #
getNetFlux <- function(rates, tol = SYBIL_SETTINGS("TOLERANCE")) {

    id  <- names(rates)
    names(rates) <- NULL

    upt <- rates < tol * -1
    prd <- rates > tol
    uusd <- logical(length(rates))
    uusd[!upt] <- TRUE
    uusd[!prd] <- TRUE

    nf <- new("netFlux",
              react_id = as.character(id),
              rate = as.numeric(rates),
              uptake = upt, product = prd, unused = uusd)

    return(nf)
}

#                            setters and getters                               #
# react_id
setMethod("react_id", signature(object = "netFlux"),
          function(object) {
              return(object@react_id)
          }
)

setReplaceMethod("react_id", signature(object = "netFlux"),
                 function(object, value) {
                     object@react_id <- value
                     return(object)
                 }
)

# rate
setMethod("rate", signature(object = "netFlux"),
          function(object) {
              return(object@rate)
          }
)

#                               other methods                                  #
setMethod("show", signature(object = "netFlux"),
    function(object) {
        ri <- react_id(object)
        rr <- rate(object)
        cat("uptake reaction rates (absolute values):\n")
        cat(sprintf(" %-20s%10f\n", ri[object@uptake], abs(rr[object@uptake])), sep = "")
        cat("\nexcretion reaction rates (absolute values):\n")
        cat(sprintf(" %-20s%10f\n", ri[object@product], rr[object@product]), sep = "")
        cat("\nunused exchange reactions [abs(rate) < ", SYBIL_SETTINGS("TOLERANCE"), "]:\n", sep = "")
        print(ri[object@unused])
    }
)

setMethod("length", signature = signature(x = "netFlux"),
          function(x) {
              return(length(rate(x)))
          }
)
