# This function performs a "n gene deletion analysis".
# In each iteration m genes are switched of (vi = 0)
# and the objective function will be computed.
# The function geneDeletion() is inspired by the functions singleGeneDeletion()
# and doubleGeneDeletion() contained in the COBRA Toolbox.
geneDeletion <- function(model, genes, combinations = 1,
                         lb = NULL, ub = NULL, checkOptSolObj = FALSE, ...) {
    if (!is(model, "modelorg")) {
        stop("needs an object of class modelorg!")
    }

    num_genes <- length(allGenes(model))

    # delGenes containes pointers to gene id's in allGenes(model)
    if (missing(genes)) {
        delGenes <- num_genes
    }
    else {
        delGenes <- genes
    }

    # make deletion matrix
    if (!is(delGenes, "matrix")) {
        delGenes <- combn(x = delGenes, m = combinations)
    }

    if (typeof(delGenes) == "character") {
        # check the id's
        dimdg    <- dim(delGenes)
        delGenes <- match(delGenes, allGenes(model))
        if (any(is.na(delGenes))) {
            stop("some gene id's are unknown, check argument genes")
        }
        else {
            attr(delGenes, which = "dim") <- dimdg
        }
    }
    else {
        if (any(delGenes > num_genes)) {
        #if ( (max(delGenes) > num_genes) || (min(delGenes) < 0) ) {
            stop("values of genes must be in [0, length(allGenes(model))]")
        }
    }

    # number of optimizations
    num_opt <- ncol(delGenes)

#                               run optimization                               #
    kogenes <- lapply(seq_len(ncol(delGenes)), function(x) delGenes[ , x])

    fd <- .generateFluxdels(model, kogenes)

    if (is.null(lb)) {
        lb <- rep(0, length(kogenes))
    }
    else {
        if (length(lb) != length(kogenes)) {
            stop("lb must be of length ", length(kogenes))
        }
    }
    if (is.null(ub)) {
        ub <- rep(0, length(kogenes))
    }
    else {
        if (length(ub) != length(kogenes)) {
            stop("ub must be of length ", length(kogenes))
        }
    }

    sol <- optimizer(model = model,
                     react = fd[["react"]],
                     lb    = lb,
                     ub    = ub,
                     ...)

    optsol <- new("optsol_genedel")
    opt <- makeOptsolMO(model, sol)
    as(optsol, "optsol_optimizeProb") <- opt

    chlb(optsol)      <- as.numeric(lb)
    chub(optsol)      <- as.numeric(ub)
    dels(optsol)      <- matrix(allGenes(model)[t(delGenes)], nrow = ncol(delGenes))
    fluxdels(optsol)  <- fd[["fd"]]
    hasEffect(optsol) <- fd[["heff"]]

    if (isTRUE(checkOptSolObj)) {
        checkOptSol(optsol, onlywarn = TRUE)
    }
    return(optsol)
}
