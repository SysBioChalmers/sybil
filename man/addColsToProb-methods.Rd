\name{addColsToProb-methods}

\docType{methods}
\encoding{utf8}

\alias{addColsToProb}
\alias{addColsToProb-methods}

\alias{addColsToProb,optObj_clpAPI-method}
\alias{addColsToProb,optObj_cplexAPI-method}
\alias{addColsToProb,optObj_glpkAPI-method}
\alias{addColsToProb,optObj_lpSolveAPI-method}

\title{Add New Columns (Variables) to an Optimization Problem}

\description{
  Add new columns (variables) to an optimization problem.
}

\usage{
\S4method{addColsToProb}{optObj_clpAPI}(lp, j, obj, lb, ub, rind, nzval)

\S4method{addColsToProb}{optObj_cplexAPI}(lp, j, obj, lb, ub, rind, nzval)

\S4method{addColsToProb}{optObj_glpkAPI}(lp, j, obj, lb, ub, rind, nzval)

\S4method{addColsToProb}{optObj_lpSolveAPI}(lp, j, obj, lb, ub, rind, nzval)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI")}}{
      method to use with package \pkg{optObj_clpAPI}.
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
  \item{j}{
    A numeric vector containing the new column indices.
  }
  \item{obj}{
    A numeric vector containing the objective coefficients of the new variables.
  }
  \item{lb}{
    A numeric vector containing the lower bounds of the new variables.
  }
  \item{ub}{
    A numeric vector containing the upper bounds of the new variables.
  }
  \item{rind}{
    A list containing the row indices of the new non-zero elements.
  }
  \item{nzval}{
    A list containing the new non-zero elements.
  }
}

\note{
  Arguments \code{j}, \code{obj}, \code{lb}, \code{lu}, \code{rind} and
  \code{nzval} must have the same length.
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
