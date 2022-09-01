#  singletonMetabolite.R
.singletonMetabolite <- function(mat, exclM, exclR) {
    if ( (missing(exclM)) || (any(is.na(exclM))) ) {
        deCandM <- logical(nrow(mat))
    }
    else {
        deCandM <- exclM
    }

    if ( (missing(exclR)) || (any(is.na(exclR))) ) {
        deCandR <- logical(ncol(mat))
    }
    else {
        deCandR <- exclR
    }

    # generate smaller matrix without singleton metabolites
    indMatM <- c(1:nrow(mat))[!deCandM]
    indMatR <- c(1:ncol(mat))[!deCandR]

    tmp_mat <- mat[!deCandM, , drop = FALSE]
    tmp_mat <- tmp_mat[ , !deCandR, drop = FALSE]

    check  <- TRUE
    sreact <- logical(ncol(tmp_mat))
    smet   <- logical(nrow(tmp_mat))

    while(isTRUE(check)) {
        rs    <- rowSums(tmp_mat)

        # row indices of reactions used only once in S
        rrs <- which(rs == 1)
        smet[rrs] <- TRUE

        if (length(rrs) > 0) {
            # get reactions (columns) using singleton metabolites
            crs <- which(colSums(tmp_mat[rrs, , drop = FALSE]) != 0)
            tmp_mat[ , crs] <- FALSE
            sreact[crs] <- TRUE
        }
        else {
            check <- FALSE
        }
    }
    SMret <- logical(nrow(mat))
    SRret <- logical(ncol(mat))
    SMret[indMatM] <- smet
    SRret[indMatR] <- sreact
    return(list(smet = SMret, sreact = SRret))
}
