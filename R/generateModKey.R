.generateModKey <- function() {
    key <- paste(sample(letters, size = 2),
                 sample(c(letters, 0:9), size = 8, replace = FALSE),
                 collapse = "", sep = "")
    return(key)
}
