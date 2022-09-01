.printLogComment <- function(reason) {
    # log comment will look like this:
    # -------------------------------------------------------- #
    ## log <reason>: Day Mon  D hh:mm:ss Year
    # -------------------------------------------------------- #
    lc1 <- paste("#", paste(rep("-", 56), collapse = ""), "#", collapse = " ")
    lc2 <- paste("## log", as.character(reason), date())
    lc  <- paste(lc1, lc2, lc1, "\n", sep = "\n")
    return(lc)
}
