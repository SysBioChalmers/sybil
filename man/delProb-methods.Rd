\name{delProb-methods}

\docType{methods}
\encoding{utf8}

\alias{delProb}
\alias{delProb-methods}

\alias{delProb,optObj_clpAPI-method}
\alias{delProb,optObj_cplexAPI-method}
\alias{delProb,optObj_glpkAPI-method}
\alias{delProb,optObj_lpSolveAPI-method}

\title{Free Memory Associated to the Pointer to the Problem Object}

\description{
  Delete (free) memory associated to the pointer to the problem object.
}

\usage{
\S4method{delProb}{optObj_clpAPI}(lp, ...)

\S4method{delProb}{optObj_cplexAPI}(lp, closeEnv = TRUE)

\S4method{delProb}{optObj_glpkAPI}(lp, ...)

\S4method{delProb}{optObj_lpSolveAPI}(lp, ...)
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
  \item{closeEnv}{
    A Boolean value. If set to \code{TRUE}, the CPLEX environment associated
    with the problem object will be closed also. Otherwise not.\cr
    Default: \code{TRUE}.
  }
  \item{...}{
    Further arguments passed to the deletion function of the solver
    package.
  }
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
