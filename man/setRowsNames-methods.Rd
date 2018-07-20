\name{setRowsNames-methods}

\docType{methods}
\encoding{utf8}

\alias{setRowsNames}
\alias{setRowsNames-methods}

\alias{setRowsNames,optObj_clpAPI,numeric,character-method}
\alias{setRowsNames,optObj_cplexAPI,numeric,character-method}
\alias{setRowsNames,optObj_glpkAPI,numeric,character-method}
\alias{setRowsNames,optObj_lpSolveAPI,numeric,character-method}

\title{Set/Change Constraint Names}

\description{
  Set or change names of constraints (rows) used in a optimization problem.
}

\usage{
\S4method{setRowsNames}{optObj_clpAPI,numeric,character}(lp, i, names)

\S4method{setRowsNames}{optObj_cplexAPI,numeric,character}(lp, i, names)

\S4method{setRowsNames}{optObj_glpkAPI,numeric,character}(lp, i, names)

\S4method{setRowsNames}{optObj_lpSolveAPI,numeric,character}(lp, i, names)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI", i = "numeric", names = "character")}}{
      method to use with package \pkg{optObj_clpAPI}.
    }
    \item{\code{signature(lp = "optObj_cplexAPI", i = "numeric", names = "character")}}{
      method to use with package \pkg{optObj_cplexAPI}.
    }
    \item{\code{signature(lp = "optObj_glpkAPI", i = "numeric", names = "character")}}{
      method to use with package \pkg{optObj_glpkAPI}.
    }
    \item{\code{signature(lp = "optObj_lpSolveAPI", i = "numeric", names = "character")}}{
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
  \item{names}{
    A character vector of the same length as \code{i} containing the
    row names.
  }
}

\value{
  \code{NULL} is invisibly returned.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  Superclass \code{\linkS4class{optObj}} and constructor function
  \code{\link{optObj}}.
}

\keyword{methods}
\keyword{optimize}
