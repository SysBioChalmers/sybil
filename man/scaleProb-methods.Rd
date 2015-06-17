\name{scaleProb-methods}

\docType{methods}
\encoding{utf8}

\alias{scaleProb}
\alias{scaleProb-methods}

\alias{scaleProb,optObj_clpAPI-method}
\alias{scaleProb,optObj_cplexAPI-method}
\alias{scaleProb,optObj_glpkAPI-method}
\alias{scaleProb,optObj_lpSolveAPI-method}

\title{Scaling of the Constraint Matrix of an Optimization Problem}

\description{
  Scaling of the constraint matrix of an optimization problem.
}

\usage{
\S4method{scaleProb}{optObj_clpAPI}(lp, opt)

\S4method{scaleProb}{optObj_cplexAPI}(lp, opt)

\S4method{scaleProb}{optObj_glpkAPI}(lp, opt)

\S4method{scaleProb}{optObj_lpSolveAPI}(lp, opt)
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
  \item{opt}{
    Scaling option depending on the used solver software.
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
