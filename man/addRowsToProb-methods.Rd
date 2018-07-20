\name{addRowsToProb-methods}

\docType{methods}
\encoding{utf8}

\alias{addRowsToProb}
\alias{addRowsToProb-methods}

\alias{addRowsToProb,optObj_clpAPI-method}
\alias{addRowsToProb,optObj_cplexAPI-method}
\alias{addRowsToProb,optObj_glpkAPI-method}
\alias{addRowsToProb,optObj_lpSolveAPI-method}

\title{Add New Rows (Constraints) to an Optimization Problem}

\description{
  Add new rows (constraints) to an optimization problem.
}

\usage{
\S4method{addRowsToProb}{optObj_clpAPI}(lp, i, type, lb, ub, cind, nzval, rnames = NULL)

\S4method{addRowsToProb}{optObj_cplexAPI}(lp, i, type, lb, ub, cind, nzval, rnames = NULL)

\S4method{addRowsToProb}{optObj_glpkAPI}(lp, i, type, lb, ub, cind, nzval, rnames = NULL)

\S4method{addRowsToProb}{optObj_lpSolveAPI}(lp, i, type, lb, ub, cind, nzval, rnames = NULL)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI")}}{
      method to use with package \pkg{optObj_clpAPI}. Parameter \code{rnames} is
      currently unused.
    }
    \item{\code{signature(lp = "optObj_cplexAPI")}}{
      method to use with package \pkg{optObj_cplexAPI}.
    }
    \item{\code{signature(lp = "optObj_glpkAPI")}}{
      method to use with package \pkg{optObj_glpkAPI}.
    }
    \item{\code{signature(lp = "optObj_lpSolveAPI")}}{
      method to use with package \pkg{optObj_lpSolveAPI}.
    }
  }
}

\arguments{
  \item{lp}{
    An object extending class \code{\linkS4class{optObj}}.
  }
  \item{i}{
    A numeric vector containing the new row indices.
  }
  \item{type}{
    A character vector giving the constraint type:
    \code{"F"}: free constraint (\pkg{optObj_glpkAPI} only),
    \code{"L"}: \code{>=} (lower bound),
    \code{"U"}: \code{<=} (upper bound) or
    \code{"D"}: \code{lb <= r <= ub} (double bound) or
    \code{"E"}: \code{=} (equality).
    If \code{type[k]} is not \code{F}, \code{"L"}, \code{"U"}, \code{"D"} or
    \code{"E"}, the value of \code{type[k]} will be set to \code{"E"}.
  }
  \item{lb}{
    A numeric vector containing the lower bound of the new constraints.
  }
  \item{ub}{
    A numeric vector containing the upper bound of the new constraints.
  }
  \item{cind}{
    A list containing the column indices of the new non-zero elements.
  }
  \item{nzval}{
    A list containing the new non-zero elements.
  }
  \item{rnames}{
    A character vector containing names for the new rows/constraints.\cr
    Default: \code{NULL}.
  }
}

\note{
  Arguments \code{i}, \code{type}, \code{lb}, \code{cind}, \code{nzval} and
  \code{rnames} (if not \code{NULL}) must have the same length.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  Superclass \code{\linkS4class{optObj}} and constructor function
  \code{\link{optObj}}.
}

\keyword{methods}
\keyword{optimize}
