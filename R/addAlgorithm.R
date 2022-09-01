addAlgorithm <- function(alg, purpose) {
    stopifnot(inherits(alg, "character"),
              inherits(purpose, "character"),
              (length(alg) == 1),
              (length(purpose) == 1))

    if (alg %in% .SYBILenv$algorithm[[purpose]]) {
        stop("algorithm ", sQuote(alg), " already exists")
    }
    else {
        .SYBILenv$algorithm[[purpose]] <- append(.SYBILenv$algorithm[[purpose]], alg)
    }
    return(invisible(NULL))
}
