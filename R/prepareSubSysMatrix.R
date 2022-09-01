.prepareSubSysMatrix <- function(reactSUBS, nreact, entrydelim = NA) {
    if (!is.null(reactSUBS)) {
        reactSsys <- as.character(reactSUBS)
        ssys_tmp <- strsplit(reactSsys, entrydelim, fixed = TRUE)
        sys_names <- unique(unlist(ssys_tmp))
        ssys <- Matrix::Matrix(FALSE,
                               nrow = length(reactSUBS),
                               ncol = length(sys_names),
                               sparse = TRUE)
        colnames(ssys) <- sys_names
        ssys_id <- mapply(match,
                          ssys_tmp,
                          MoreArgs = list(sys_names),
                          SIMPLIFY = FALSE)

        for (i in seq(along = ssys_id)) {
            if (length(ssys_id[[i]]) > 0) {
                ssys[i, ssys_id[[i]]] <- TRUE
            }
        }

    }
    else {
        ssys <- Matrix::Matrix(FALSE, nrow = nreact, ncol = 1, sparse = TRUE)
        colnames(ssys) <- NA
    }
    return(ssys)
}
