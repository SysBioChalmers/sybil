\name{getRowsNames-methods}

\docType{methods}
\encoding{utf8}

\alias{getRowsNames}
\alias{getRowsNames-methods}

\alias{getRowsNames,optObj_cplexAPI,numeric-method}
\alias{getRowsNames,optObj_glpkAPI,numeric-method}
\alias{getRowsNames,optObj_lpSolveAPI,numeric-method}

\title{Retrieve Constraint Names}

\description{
  Get names of constraints (rows) used in a optimization problem.
}

\usage{
\S4method{getRowsNames}{optObj_cplexAPI,numeric}(lp, i)

\S4method{getRowsNames}{optObj_glpkAPI,numeric}(lp, i)

\S4method{getRowsNames}{optObj_lpSolveAPI,numeric}(lp, i)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_cplexAPI", i = "numeric")}}{
      method to use with package \pkg{optObj_cplexAPI}.
    }
    \item{\code{signature(lp = "optObj_glpkAPI", i = "numeric")}}{
      method to use with package \pkg{optObj_glpkAPI}.
    }
    \item{\code{signature(lp = "optObj_lpSolveAPI", i = "numeric")}}{
      method to use with package \pkg{optObj_lpSolveAPI}.
    }
  }
}

\arguments{
  \item{lp}{
    An object extending class \code{\linkS4class{optObj}}.
  }
  \item{i}{
    A numeric vector of row indices.
  }
}

\value{
  A character vector of row names, if names are existing.
}

\note{
  For the \code{\linkS4class{optObj_glpkAPI}} method: the result vector may
  be shorter than \code{i}, if some names are missing.
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
