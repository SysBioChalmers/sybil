\name{getRowsUppBnds-methods}

\docType{methods}
\encoding{utf8}

\alias{getRowsUppBnds}
\alias{getRowsUppBnds-methods}

\alias{getRowsUppBnds,optObj_clpAPI,numeric-method}
\alias{getRowsUppBnds,optObj_cplexAPI,numeric-method}
\alias{getRowsUppBnds,optObj_glpkAPI,numeric-method}
\alias{getRowsUppBnds,optObj_lpSolveAPI,numeric-method}

\title{Get Upper Bounds of the Rows (Constraints) of the Optimization Problem}

\description{
  Get upper bounds of the rows (constraints) of the optimization Problem.
}

\usage{
\S4method{getRowsUppBnds}{optObj_clpAPI,numeric}(lp, i)

\S4method{getRowsUppBnds}{optObj_cplexAPI,numeric}(lp, i)

\S4method{getRowsUppBnds}{optObj_glpkAPI,numeric}(lp, i)

\S4method{getRowsUppBnds}{optObj_lpSolveAPI,numeric}(lp, i)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI", i = "numeric")}}{
      method to use with package \pkg{optObj_clpAPI}.
    }
    \item{\code{signature(lp = "optObj_cplexAPI", i = "numeric")}}{
      method to use with package \pkg{optObj_cplexAPI}. This method returns
      always \code{FALSE}.
    }
    \item{\code{signature(lp = "optObj_glpkAPI", i = "numeric")}}{
      method to use with package \pkg{optObj_glpkAPI}.
    }
    \item{\code{signature(lp = "optObj_lpSolveAPI", i = "numeric")}}{
      method to use with package \pkg{optObj_lpSolveAPI}.
    }
  }
}

\arguments{
  \item{lp}{
    An object extending class \code{\linkS4class{optObj}}.
  }
  \item{i}{
    A numeric vector containing the row indices.
  }
}

\value{
  A numeric vector containing the desired row bounds.
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
