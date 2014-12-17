\name{getColPrim-methods}

\docType{methods}
\encoding{utf8}

\alias{getColPrim}
\alias{getColPrim-methods}

\alias{getColPrim,optObj_clpAPI,numeric-method}
\alias{getColPrim,optObj_cplexAPI,numeric-method}
\alias{getColPrim,optObj_glpkAPI,numeric-method}
\alias{getColPrim,optObj_lpSolveAPI,numeric-method}

\title{Get Primal Value of Variables After Optimization}

\description{
  Get primal value of variables after optimization.
}

\usage{
\S4method{getColPrim}{optObj_clpAPI,numeric}(lp, j)

\S4method{getColPrim}{optObj_cplexAPI,numeric}(lp, j)

\S4method{getColPrim}{optObj_glpkAPI,numeric}(lp, j)

\S4method{getColPrim}{optObj_lpSolveAPI,numeric}(lp, j)
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
  A numeric vector containing the desired primal values.
}

\author{
  Gabriel Gelius-Dietrich
}

\seealso{
  Superclass \code{\linkS4class{optObj}} and constructor function
  \code{\link{optObj}}.
}

\keyword{methods}
\keyword{optimize}
