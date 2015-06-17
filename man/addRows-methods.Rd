\name{addRows-methods}

\docType{methods}
\encoding{utf8}

\alias{addRows}
\alias{addRows-methods}

\alias{addRows,optObj_clpAPI,numeric-method}
\alias{addRows,optObj_cplexAPI,numeric-method}
\alias{addRows,optObj_glpkAPI,numeric-method}
\alias{addRows,optObj_lpSolveAPI,numeric-method}

\title{Add Rows to an Optimization Problem}

\description{
  Add rows to an optimization problem.
}

\usage{
\S4method{addRows}{optObj_clpAPI,numeric}(lp, nrows)

\S4method{addRows}{optObj_cplexAPI,numeric}(lp, nrows)

\S4method{addRows}{optObj_glpkAPI,numeric}(lp, nrows)

\S4method{addRows}{optObj_lpSolveAPI,numeric}(lp, nrows)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI", nrows = "numeric")}}{
      method to use with package \pkg{optObj_clpAPI}.
    }
    \item{\code{signature(lp = "optObj_cplexAPI", nrows = "numeric")}}{
      method to use with package \pkg{optObj_cplexAPI}.
    }
    \item{\code{signature(lp = "optObj_glpkAPI", nrows = "numeric")}}{
      method to use with package \pkg{optObj_glpkAPI}.
    }
    \item{\code{signature(lp = "optObj_lpSolveAPI", nrows = "numeric")}}{
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
