\name{setRhsZero-methods}

\docType{methods}
\encoding{utf8}

\alias{setRhsZero}
\alias{setRhsZero-methods}

\alias{setRhsZero,optObj_clpAPI-method}
\alias{setRhsZero,optObj_cplexAPI-method}
\alias{setRhsZero,optObj_glpkAPI-method}
\alias{setRhsZero,optObj_lpSolveAPI-method}

\title{Set Right Hand Side of the Optimization Problem To Zero}

\description{
  Set right hand side of the optimization problem to zero:
  \eqn{\mbox{\boldmath$Sv$\unboldmath} = 0}{Sv = 0}.
}

\usage{
\S4method{setRhsZero}{optObj_clpAPI}(lp)

\S4method{setRhsZero}{optObj_cplexAPI}(lp)

\S4method{setRhsZero}{optObj_glpkAPI}(lp)

\S4method{setRhsZero}{optObj_lpSolveAPI}(lp)
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


\seealso{
  Superclass \code{\linkS4class{optObj}} and constructor function
  \code{\link{optObj}}.
}

\keyword{methods}
\keyword{optimize}
