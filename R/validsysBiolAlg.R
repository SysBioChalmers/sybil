# Validity checking of an object of class sysBiolAlg.
.validsysBiolAlg <- function(object) {
    if (!is(object, "sysBiolAlg")) {
        return("needs an object of class sysBiolAlg")
    }
#    if (length(fldind(object)) != 2) {
#        return("slot fluxdist must have a length of 2")
#    }
    return(TRUE)
}
