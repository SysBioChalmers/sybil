\name{getColsLowBnds-methods}

\docType{methods}
\encoding{utf8}

\alias{getColsLowBnds}
\alias{getColsLowBnds-methods}

\alias{getColsLowBnds,optObj_clpAPI,numeric-method}
\alias{getColsLowBnds,optObj_cplexAPI,numeric-method}
\alias{getColsLowBnds,optObj_glpkAPI,numeric-method}
\alias{getColsLowBnds,optObj_lpSolveAPI,numeric-method}

\title{Get Lower Bounds of the Columns (Variables) of the Optimization Problem}

\description{
  Get lower bounds of the columns (variables) of the optimization Problem.
}

\usage{
\S4method{getColsLowBnds}{optObj_clpAPI,numeric}(lp, j)

\S4method{getColsLowBnds}{optObj_cplexAPI,numeric}(lp, j)

\S4method{getColsLowBnds}{optObj_glpkAPI,numeric}(lp, j)

\S4method{getColsLowBnds}{optObj_lpSolveAPI,numeric}(lp, j)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI", j = "numeric")}}{
      method to use with package \pkg{optObj_clpAPI}.
    }
    \item{\code{signature(lp = "optObj_cplexAPI", j = "numeric")}}{
      method to use with package \pkg{optObj_cplexAPI}.
    }
    \item{\code{signature(lp = "optObj_glpkAPI", j = "numeric")}}{
      method to use with package \pkg{optObj_glpkAPI}.
    }
    \item{\code{signature(lp = "optObj_lpSolveAPI", j = "numeric")}}{
      method to use with package \pkg{optObj_lpSolveAPI}.
    }
  }
}

\arguments{
  \item{lp}{
    An object extending class \code{\linkS4class{optObj}}.
  }
  \item{j}{
    A numeric vector containing the column (variable) indices.
  }
}

\value{
  A numeric vector containing the desired column bounds.
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
