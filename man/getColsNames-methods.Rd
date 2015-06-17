\name{getColsNames-methods}

\docType{methods}
\encoding{utf8}

\alias{getColsNames}
\alias{getColsNames-methods}

\alias{getColsNames,optObj_cplexAPI,numeric-method}
\alias{getColsNames,optObj_glpkAPI,numeric-method}
\alias{getColsNames,optObj_lpSolveAPI,numeric-method}

\title{Retrieve Variable Names}

\description{
  Get names of variables (columns) used in a optimization problem.
}

\usage{
\S4method{getColsNames}{optObj_cplexAPI,numeric}(lp, j)

\S4method{getColsNames}{optObj_glpkAPI,numeric}(lp, j)

\S4method{getColsNames}{optObj_lpSolveAPI,numeric}(lp, j)
}

\section{Methods}{
  \describe{
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
    A numeric vector of column indices.
  }
}

\value{
  A character vector of column names, if names are existing.
}

\note{
  For the \code{\linkS4class{optObj_glpkAPI}} method: the result vector may
  be shorter than \code{j}, if some names are missing.
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
