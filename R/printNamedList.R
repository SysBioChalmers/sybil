.printNamedList <- function(nList, ...) {
      na <- names(nList)
      for (i in seq(along = na)) {
          cat(sprintf("%-20s", na[i]), deparse(nList[[i]]), "\n", ...)
      }
}
