\name{changeMatrixRow-methods}

\docType{methods}
\encoding{utf8}

\alias{changeMatrixRow}
\alias{changeMatrixRow-methods}

\alias{changeMatrixRow,optObj_cplexAPI-method}
\alias{changeMatrixRow,optObj_glpkAPI-method}
\alias{changeMatrixRow,optObj_lpSolveAPI-method}

\title{Change a Row in the Constraint Matrix of the Optimization Problem}

\description{
  Change a row in the constraint matrix of the optimization problem.
}

\usage{
\S4method{changeMatrixRow}{optObj_cplexAPI}(lp, i, j, val)

\S4method{changeMatrixRow}{optObj_glpkAPI}(lp, i, j, val)

\S4method{changeMatrixRow}{optObj_lpSolveAPI}(lp, i, j, val)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_cplexAPI")}}{
      method to use with package \pkg{optObj_cplexAPI}. Only the columns given
      in argument \code{j} will be changed. All other columns stay the same.
    }
    \item{\code{signature(lp = "optObj_glpkAPI")}}{
      method to use with package \pkg{optObj_glpkAPI}. The row given in argument
      \code{i} will be reset completely.
    }
    \item{\code{signature(lp = "optObj_lpSolveAPI")}}{
      method to use with package \pkg{optObj_lpSolveAPI}. The row given in
      argument \code{i} will be reset completely.
    }
  }
}

\arguments{
  \item{lp}{
    An object extending class \code{\linkS4class{optObj}}.
  }
  \item{i}{
    A single numeric value giving the row index of the constraint matrix to
    change.
  }
  \item{j}{
    A numeric vector containing the column indices of the new non-zero elements.
  }
  \item{val}{
    A numeric vector of the same length as \code{j} containing the new non-zero
    elements.
  }
}

\author{
  Gabriel Gelius-Dietrich
}

\seealso{
  Superclass \code{\linkS4class{optObj}} and constructor function
  \code{\link{optObj}}.
}

\keyword{methods}
\keyword{optimize}
