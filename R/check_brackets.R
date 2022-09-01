# check, whether we have the same number of '(' and ')'.
.check_brackets <- function(rule) {
    i  <- 1
    nb <- 0
    nc <- length(rule)
    while ( (nb >= 0) && (i <= nc) ) {
        switch(rule[i],
            "(" = { nb <- nb + 1 },
            ")" = { nb <- nb - 1 }
        )
        i <- i + 1
    }
    return( ifelse(nb == 0, TRUE, FALSE) )
}
