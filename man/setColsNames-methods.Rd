\name{setColsNames-methods}

\docType{methods}
\encoding{utf8}

\alias{setColsNames}
\alias{setColsNames-methods}

\alias{setColsNames,optObj_clpAPI,numeric,character-method}
\alias{setColsNames,optObj_cplexAPI,numeric,character-method}
\alias{setColsNames,optObj_glpkAPI,numeric,character-method}
\alias{setColsNames,optObj_lpSolveAPI,numeric,character-method}

\title{Set/Change Variable Names}

\description{
  Set or change names of variables (columns) used in a optimization problem.
}

\usage{
\S4method{setColsNames}{optObj_clpAPI,numeric,character}(lp, j, names)

\S4method{setColsNames}{optObj_cplexAPI,numeric,character}(lp, j, names)

\S4method{setColsNames}{optObj_glpkAPI,numeric,character}(lp, j, names)

\S4method{setColsNames}{optObj_lpSolveAPI,numeric,character}(lp, j, names)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI", j = "numeric", names = "character")}}{
      method to use with package \pkg{optObj_clpAPI}.
    }
    \item{\code{signature(lp = "optObj_cplexAPI", j = "numeric", names = "character")}}{
      method to use with package \pkg{optObj_cplexAPI}.
    }
    \item{\code{signature(lp = "optObj_glpkAPI", j = "numeric", names = "character")}}{
      method to use with package \pkg{optObj_glpkAPI}.
    }
    \item{\code{signature(lp = "optObj_lpSolveAPI", j = "numeric", names = "character")}}{
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
  \item{names}{
    A character vector of the same length as \code{j} containing the
    column names.
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
