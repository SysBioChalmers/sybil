\name{solveLp-methods}

\docType{methods}
\encoding{utf8}

\alias{solveLp}
\alias{solveLp-methods}

\alias{solveLp,optObj_clpAPI-method}
\alias{solveLp,optObj_cplexAPI-method}
\alias{solveLp,optObj_glpkAPI-method}
\alias{solveLp,optObj_lpSolveAPI-method}

\title{Optimize Problem Object}

\description{
  Optimize problem object.
}

\usage{
\S4method{solveLp}{optObj_clpAPI}(lp)

\S4method{solveLp}{optObj_cplexAPI}(lp)

\S4method{solveLp}{optObj_glpkAPI}(lp)

\S4method{solveLp}{optObj_lpSolveAPI}(lp)
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
