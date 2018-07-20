\name{changeRowsBnds-methods}

\docType{methods}
\encoding{utf8}

\alias{changeRowsBnds}
\alias{changeRowsBnds-methods}

\alias{changeRowsBnds,optObj_clpAPI-method}
\alias{changeRowsBnds,optObj_cplexAPI-method}
\alias{changeRowsBnds,optObj_glpkAPI-method}
\alias{changeRowsBnds,optObj_lpSolveAPI-method}

\title{Change Row Bounds in the Optimization Problem}

\description{
  Change row bounds in the optimization problem.
}

\usage{
\S4method{changeRowsBnds}{optObj_clpAPI}(lp, i, lb, ub)

\S4method{changeRowsBnds}{optObj_cplexAPI}(lp, i, lb, ub)

\S4method{changeRowsBnds}{optObj_glpkAPI}(lp, i, lb, ub)

\S4method{changeRowsBnds}{optObj_lpSolveAPI}(lp, i, lb, ub)
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
  \item{i}{
    A numeric vector containing the row indices of the constraints to change.
  }
  \item{lb}{
    A numeric vector of the same length as \code{i} containing the lower bounds
    of the constraints to change.
  }
  \item{ub}{
    A numeric vector of the same length as \code{i} containing the upper bounds
    of the constraints to change.
  }
}

\note{
  Changing row bounds does not change the constraint type.
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
