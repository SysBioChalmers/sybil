\name{getObjCoefs-methods}

\docType{methods}
\encoding{utf8}

\alias{getObjCoefs}
\alias{getObjCoefs-methods}

\alias{getObjCoefs,optObj_clpAPI,numeric-method}
\alias{getObjCoefs,optObj_cplexAPI,numeric-method}
\alias{getObjCoefs,optObj_glpkAPI,numeric-method}
\alias{getObjCoefs,optObj_lpSolveAPI,numeric-method}

\title{Get Objective Coefficients of the Optimization Problem}

\description{
  Get objective coefficients of the optimization problem.
}

\usage{
\S4method{getObjCoefs}{optObj_clpAPI,numeric}(lp, j)

\S4method{getObjCoefs}{optObj_cplexAPI,numeric}(lp, j)

\S4method{getObjCoefs}{optObj_glpkAPI,numeric}(lp, j)

\S4method{getObjCoefs}{optObj_lpSolveAPI,numeric}(lp, j)
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
  A numeric vector containing the desired objective coefficients.
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
