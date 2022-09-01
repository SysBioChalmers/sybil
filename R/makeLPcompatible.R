# generates names, compatible to CPLEX LP file format:
# a name must not start with a period or digit and must not contain
# square brackets
.makeLPcompatible <- function(name, prefix, sep = "") {
    nameLP <- sub("[", "(", name,   fixed = TRUE)
    nameLP <- sub("]", ")", nameLP, fixed = TRUE)
    nameLP <- sub("^([0-9\\.])", paste(prefix, "\\1", sep = sep),
                  nameLP, perl = TRUE)
    #nameLP <- paste(prefix, nameLP, sep = sep) # prefix = "M_" or "R_"
    return(nameLP)
}
