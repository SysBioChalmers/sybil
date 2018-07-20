\name{initProb-methods}

\docType{methods}
\encoding{utf8}

\alias{initProb}
\alias{initProb-methods}

\alias{initProb,optObj_clpAPI-method}
\alias{initProb,optObj_cplexAPI-method}
\alias{initProb,optObj_glpkAPI-method}
\alias{initProb,optObj_lpSolveAPI-method}

\title{Initialize Problem Object}

\description{
  Initialize Problem Object.
}

\usage{
\S4method{initProb}{optObj_clpAPI}(lp, to = NULL, ...)

\S4method{initProb}{optObj_cplexAPI}(lp, to = FALSE, ...)

\S4method{initProb}{optObj_glpkAPI}(lp, to = FALSE, ...)

\S4method{initProb}{optObj_lpSolveAPI}(lp, to = NULL, nrows, ncols)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI")}}{
      method to use with package \pkg{optObj_clpAPI}, argument \code{to} can
      be a single numeric value: \code{0} -- \dQuote{none},
      \code{1} -- \dQuote{just final}, \code{2} -- \dQuote{just factorizations},
      \code{3} -- \dQuote{as 2 plus a bit more}, code{4} -- \dQuote{verbose}.
      See COIN-OR Clp documentation for more details.
    }
    \item{\code{signature(lp = "optObj_cplexAPI")}}{
      method to use with package \pkg{optObj_cplexAPI}, argument \code{to} can
      be \code{TRUE} or \code{FALSE}. Setting CPLEX parameter
      \code{CPX_PARAM_SCRIND} to \code{CPX_ON} or \code{CPX_OFF} has the same
      effect.
    }
    \item{\code{signature(lp = "optObj_glpkAPI")}}{
      method to use with package \pkg{optObj_glpkAPI}, argument \code{to} can
      be \code{TRUE} or \code{FALSE}, setting GLPK function
      \code{termOutGLPK} to \code{GLP_ON} or \code{GLP_OFF}. The amount of
      output is controlled by the GLPK parameter \code{MSG_LEV}.
    }
    \item{\code{signature(lp = "optObj_lpSolveAPI")}}{
      method to use with package \pkg{optObj_lpSolveAPI}, argument \code{to} can
      be a single character value, see \pkg{lpSolveAPI} documentation for more
      details (\code{lp.control.options}, section \code{verbose}).
    }
  }
}

\arguments{
  \item{lp}{
    An object extending class \code{\linkS4class{optObj}}.
  }
  \item{to}{
      A single boolean, numeric or character value, controling the amount of
      terminal output of the solver software.\cr
      Default: \code{FALSE} or \code{NULL}.
  }
  \item{nrows}{
    Number of rows (constraints) of the new problem object.
  }
  \item{ncols}{
    Number of columns (variables) of the new problem object.
  }
  \item{...}{
    Further arguments passed to the initialization function of the solver
    package.
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
