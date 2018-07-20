\name{addRowsCols-methods}

\docType{methods}
\encoding{utf8}

\alias{addRowsCols}
\alias{addRowsCols-methods}

\alias{addRowsCols,optObj_clpAPI,numeric,numeric-method}
\alias{addRowsCols,optObj_cplexAPI,numeric,numeric-method}
\alias{addRowsCols,optObj_glpkAPI,numeric,numeric-method}
\alias{addRowsCols,optObj_lpSolveAPI,numeric,numeric-method}

\title{Add Rows and Columns to an Optimization Problem}

\description{
  Add rows and columns to an optimization problem.
}

\usage{
\S4method{addRowsCols}{optObj_clpAPI,numeric,numeric}(lp, nrows, ncols)

\S4method{addRowsCols}{optObj_cplexAPI,numeric,numeric}(lp, nrows, ncols)

\S4method{addRowsCols}{optObj_glpkAPI,numeric,numeric}(lp, nrows, ncols)

\S4method{addRowsCols}{optObj_lpSolveAPI,numeric,numeric}(lp, nrows, ncols)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI", nrows = "numeric", ncols = "numeric")}}{
      method to use with package \pkg{optObj_clpAPI}.
    }
    \item{\code{signature(lp = "optObj_cplexAPI", nrows = "numeric", ncols = "numeric")}}{
      method to use with package \pkg{optObj_cplexAPI}.
    }
    \item{\code{signature(lp = "optObj_glpkAPI", nrows = "numeric", ncols = "numeric")}}{
      method to use with package \pkg{optObj_glpkAPI}.
    }
    \item{\code{signature(lp = "optObj_lpSolveAPI", nrows = "numeric", ncols = "numeric")}}{
      method to use with package \pkg{optObj_lpSolveAPI}.
    }
  }
}

\arguments{
  \item{lp}{
    An object extending class \code{\linkS4class{optObj}}.
  }
  \item{nrows}{
    Number of rows (constraints) to add to the problem object.
  }
  \item{ncols}{
    Number of columns (variables) to add to the problem object.
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
