checkAlgorithm <- function(alg, purpose) {
    return(alg %in% .SYBILenv$algorithm[[purpose]])
}
