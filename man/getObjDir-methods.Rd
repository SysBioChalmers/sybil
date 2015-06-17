\name{getObjDir-methods}

\docType{methods}
\encoding{utf8}

\alias{getObjDir}
\alias{getObjDir-methods}

\alias{getObjDir,optObj_clpAPI-method}
\alias{getObjDir,optObj_cplexAPI-method}
\alias{getObjDir,optObj_glpkAPI-method}
\alias{getObjDir,optObj_lpSolveAPI-method}

\title{Get Direction of Optimization.}

\description{
  Get direction of optimization.
}

\usage{
\S4method{getObjDir}{optObj_clpAPI}(lp)

\S4method{getObjDir}{optObj_cplexAPI}(lp)

\S4method{getObjDir}{optObj_glpkAPI}(lp)

\S4method{getObjDir}{optObj_lpSolveAPI}(lp)
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

\value{
  Returns a single character string indicating the direction of optimization:
  \code{"max"}: maximization, or \code{"min"}: minimization.
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
