.doInRound <- function(indRounds, maxRounds) {
    if (maxRounds < 1) {
        stop("Argument 'maxRounds' must be > 0")
    }

    if (is.null(indRounds)) {
        runPP <- c(1:maxRounds)
    }
    else {
        if (is(indRounds, "character")) {
            # make indRounds integer values
            DIR <- as.integer(indRounds)

            # first value is treated as offset, if indRounds
            # contains two elements (more than two elements are ignored)
            if (length(DIR) > 1) {
                offs <- DIR[1]
                DIR  <- ifelse(DIR[2] < 1, 1L, abs(DIR[2]))
            }
            else {
                offs <- 0L
                DIR  <- ifelse(DIR[1] < 1, 1L, abs(DIR[1]))
            }

            # when we will run pre/post processing
            runPP <- seq(from = (DIR+offs), to = maxRounds, by = DIR)
            runPP <- runPP[runPP > 0]

        }
        else if (is(indRounds, "numeric")) {
            runPP <- sort(as.integer(indRounds[indRounds > 0]))
        }
        else {
            warning("Argument 'indRounds' must be numeric or character")
            runPP <- c(1:maxRounds)
        }
    }
    return(runPP)
}
