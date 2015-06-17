\name{getRedCosts-methods}

\docType{methods}
\encoding{utf8}

\alias{getRedCosts}
\alias{getRedCosts-methods}

\alias{getRedCosts,optObj_clpAPI-method}
\alias{getRedCosts,optObj_cplexAPI-method}
\alias{getRedCosts,optObj_glpkAPI-method}
\alias{getRedCosts,optObj_lpSolveAPI-method}

\title{Get Reduced Costs of all Variables After Optimization}

\description{
  Get reduced costs of all variables after optimization.
}

\usage{
\S4method{getRedCosts}{optObj_clpAPI}(lp)

\S4method{getRedCosts}{optObj_cplexAPI}(lp)

\S4method{getRedCosts}{optObj_glpkAPI}(lp)

\S4method{getRedCosts}{optObj_lpSolveAPI}(lp)
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

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\value{
  A numeric vector containing the reduced costs of all variables.
}

\seealso{
  Superclass \code{\linkS4class{optObj}} and constructor function
  \code{\link{optObj}}.
}

\keyword{methods}
\keyword{optimize}
