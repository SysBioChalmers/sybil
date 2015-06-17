\name{addCols-methods}

\docType{methods}
\encoding{utf8}

\alias{addCols}
\alias{addCols-methods}

\alias{addCols,optObj_clpAPI,numeric-method}
\alias{addCols,optObj_cplexAPI,numeric-method}
\alias{addCols,optObj_glpkAPI,numeric-method}
\alias{addCols,optObj_lpSolveAPI,numeric-method}

\title{Add Columns to an Optimization Problem}

\description{
  Add columns to an optimization problem.
}

\usage{
\S4method{addCols}{optObj_clpAPI,numeric}(lp, ncols)

\S4method{addCols}{optObj_cplexAPI,numeric}(lp, ncols)

\S4method{addCols}{optObj_glpkAPI,numeric}(lp, ncols)

\S4method{addCols}{optObj_lpSolveAPI,numeric}(lp, ncols)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI", ncols = "numeric")}}{
      method to use with package \pkg{optObj_clpAPI}.
    }
    \item{\code{signature(lp = "optObj_cplexAPI", ncols = "numeric")}}{
      method to use with package \pkg{optObj_cplexAPI}.
    }
    \item{\code{signature(lp = "optObj_glpkAPI", ncols = "numeric")}}{
      method to use with package \pkg{optObj_glpkAPI}.
    }
    \item{\code{signature(lp = "optObj_lpSolveAPI", ncols = "numeric")}}{
      method to use with package \pkg{optObj_lpSolveAPI}.
    }
  }
}

\arguments{
  \item{lp}{
    An object extending class \code{\linkS4class{optObj}}.
  }
  \item{ncols}{
    Number of columns (variables) to add to the problem object.
  }
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  Superclass \code{\linkS4class{optObj}} and constructor function
  \code{\link{optObj}}.
}

\keyword{methods}
\keyword{optimize}
