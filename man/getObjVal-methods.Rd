\name{getObjVal-methods}

\docType{methods}
\encoding{utf8}

\alias{getObjVal}
\alias{getObjVal-methods}

\alias{getObjVal,optObj_clpAPI-method}
\alias{getObjVal,optObj_cplexAPI-method}
\alias{getObjVal,optObj_glpkAPI-method}
\alias{getObjVal,optObj_lpSolveAPI-method}

\title{Get Value of the Objective Function After Optimization}

\description{
  Get value of the objective function after optimization.
}

\usage{
\S4method{getObjVal}{optObj_clpAPI}(lp)

\S4method{getObjVal}{optObj_cplexAPI}(lp)

\S4method{getObjVal}{optObj_glpkAPI}(lp)

\S4method{getObjVal}{optObj_lpSolveAPI}(lp)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI")}}{
      method to use with package \pkg{optObj_clpAPI}.
    }
    \item{\code{signature(lp = "optObj_cplexAPI")}}{
      method to use with package \pkg{optObj_cplexAPI}. For problems of type
      \code{"mip"}: if no solution exists, the \pkg{cplexAPI} function
      \code{getBestObjValCPLEX} will be used.
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
  Returns a single numeric value.
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
