# mat:   the matrix to recode (class Matrix)
# signs: character vector of length three:
#        mat[i, j]  > tol    -->   signs[3]
#        mat[i, j]  < tol    -->   signs[1]
#        mat[i, j] == tol    -->   signs[2]
# tol:   tolerance value
.recodeMatrix <- function(mat,
                          signs = c("-", " ", "+"),
                          tol = SYBIL_SETTINGS("TOLERANCE")) {

    stopifnot(is(mat, "Matrix"), length(signs) == 3)
    mat <- apply(mat, 2,
                 function(x) {
                     ifelse(x > tol, signs[3], ifelse(abs(x) > tol,
                                                      signs[1], signs[2]))
                 })
    return(mat)
}
