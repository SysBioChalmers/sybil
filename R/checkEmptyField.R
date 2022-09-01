.checkEmptyField <- function(vec, name) {

    nalines <- which(is.na(vec) | nchar(vec, allowNA = TRUE) == 0)
    if (length(nalines) > 0) {
        msg <- sprintf(ngettext(length(nalines),
                                paste(" for", sQuote(name), "in %d line: %s"),
                                paste("s for", sQuote(name), "in %d lines: %s")),
                       length(nalines), paste(nalines+1, collapse = ", "))
        msg <- paste("empty field", msg, "\n", sep = "")
        out <- list(nalines = nalines, msg = msg)
    }
    else {
        out <- NULL
    }
    return(out)
}
