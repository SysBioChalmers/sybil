\name{getNumCols-methods}

\docType{methods}
\encoding{utf8}

\alias{getNumCols}
\alias{getNumCols-methods}

\alias{getNumCols,optObj_clpAPI-method}
\alias{getNumCols,optObj_cplexAPI-method}
\alias{getNumCols,optObj_glpkAPI-method}
\alias{getNumCols,optObj_lpSolveAPI-method}

\title{Get Number of Columns (Variables) of the Optimization Problem}

\description{
  Get number of columns (variables) of the optimization problem.
}

\usage{
\S4method{getNumCols}{optObj_clpAPI}(lp)

\S4method{getNumCols}{optObj_cplexAPI}(lp)

\S4method{getNumCols}{optObj_glpkAPI}(lp)

\S4method{getNumCols}{optObj_lpSolveAPI}(lp)
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
