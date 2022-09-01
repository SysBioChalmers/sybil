.generateFluxdels <- function(model, geneList) {
    message("compute affected fluxes ... ", appendLF = FALSE)
    react <- mapply(geneDel, geneList,
                    MoreArgs = list(model = model), SIMPLIFY = FALSE)
    heff  <- ! sapply(react, is.null, simplify = TRUE, USE.NAMES = FALSE)
    fd       <- vector(mode = "list", length = length(react))
    fd[heff] <- lapply(react[heff], function(x) react_id(model)[x])
    message("OK")
    return(list(react = react, heff = heff, fd = fd))
}
