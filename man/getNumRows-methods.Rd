\name{getNumRows-methods}

\docType{methods}
\encoding{utf8}

\alias{getNumRows}
\alias{getNumRows-methods}

\alias{getNumRows,optObj_clpAPI-method}
\alias{getNumRows,optObj_cplexAPI-method}
\alias{getNumRows,optObj_glpkAPI-method}
\alias{getNumRows,optObj_lpSolveAPI-method}

\title{Get Number of Rows (Constraints) of the Optimization Problem}

\description{
  Get number of rows (constraints) of the optimization problem.
}

\usage{
\S4method{getNumRows}{optObj_clpAPI}(lp)

\S4method{getNumRows}{optObj_cplexAPI}(lp)

\S4method{getNumRows}{optObj_glpkAPI}(lp)

\S4method{getNumRows}{optObj_lpSolveAPI}(lp)
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

\value{
  A single numeric value.
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
