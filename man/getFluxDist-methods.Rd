\name{getFluxDist-methods}

\docType{methods}
\encoding{utf8}

\alias{getFluxDist}
\alias{getFluxDist-methods}

\alias{getFluxDist,optObj_clpAPI-method}
\alias{getFluxDist,optObj_cplexAPI-method}
\alias{getFluxDist,optObj_glpkAPI-method}
\alias{getFluxDist,optObj_lpSolveAPI-method}
\alias{getFluxDist,optsol-method}

\title{Retrieve Flux Distribution}

\description{
  Get all primal values of variables after optimization (the resulting flux
  distribution).
}

\usage{
\S4method{getFluxDist}{optObj_clpAPI}(lp)

\S4method{getFluxDist}{optObj_cplexAPI}(lp)

\S4method{getFluxDist}{optObj_glpkAPI}(lp)

\S4method{getFluxDist}{optObj_lpSolveAPI}(lp)

\S4method{getFluxDist}{optsol}(lp, react = NULL, opt = NULL, drop = TRUE)
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
    \item{\code{signature(lp = "optsol")}}{
      method to use with objects of class \code{\linkS4class{optsol}}.
      Returns a subset of the flux distribution stored in slot \code{fluxdist}
      as object of class \code{\link[Matrix]{Matrix}}.
      If arguments \code{react} and \code{opt} are both set to \code{NULL}
      (default), the flux distribution corresponding to the variable indices in
      slot \code{fldind} will be returned.
    }
  }
}

\arguments{
  \item{lp}{
    An object extending class \code{\linkS4class{optObj}} or class
    \code{\linkS4class{optsol}}.
  }
  \item{react}{
    Numeric vector or object of class \code{\linkS4class{reactId}} indicating
    the reactions (rows of the flux distribution) to return.\cr
    Default: \code{NULL}.
  }
  \item{opt}{
    Numeric vector indicating the optimizations (columns of the flux
    distribution) to return.\cr
    Default: \code{NULL}.
  }
  \item{drop}{
    Used for array subsetting like in \code{\link{[}}.\cr
    Default: \code{TRUE}.
  }
}

\value{
  A numeric matrix or vector containing all primal values
  (the flux distribution).
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
