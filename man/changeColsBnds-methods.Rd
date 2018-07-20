\name{changeColsBnds-methods}

\docType{methods}
\encoding{utf8}

\alias{changeColsBnds}
\alias{changeColsBnds-methods}

\alias{changeColsBnds,optObj_clpAPI-method}
\alias{changeColsBnds,optObj_cplexAPI-method}
\alias{changeColsBnds,optObj_glpkAPI-method}
\alias{changeColsBnds,optObj_lpSolveAPI-method}

\title{Change Column (Variable) Bounds in the Optimization Problem}

\description{
  Change column (variable) bounds in the optimization problem.
}

\usage{
\S4method{changeColsBnds}{optObj_clpAPI}(lp, j, lb, ub)

\S4method{changeColsBnds}{optObj_cplexAPI}(lp, j, lb, ub)

\S4method{changeColsBnds}{optObj_glpkAPI}(lp, j, lb, ub)

\S4method{changeColsBnds}{optObj_lpSolveAPI}(lp, j, lb, ub)
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
    A numeric vector containing the column indices of the variables to change.
  }
  \item{lb}{
    A numeric vector of the same length as \code{j} containing the lower bounds
    of the variables to change.
  }
  \item{ub}{
    A numeric vector of the same length as \code{j} containing the upper bounds
    of the variables to change.
  }
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
