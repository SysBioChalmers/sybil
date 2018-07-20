\name{getNumNnz-methods}

\docType{methods}
\encoding{utf8}

\alias{getNumNnz}
\alias{getNumNnz-methods}

\alias{getNumNnz,optObj_clpAPI-method}
\alias{getNumNnz,optObj_cplexAPI-method}
\alias{getNumNnz,optObj_glpkAPI-method}

\title{Retrieve the Number of Non-Zero Elements of the Constraint Matrix}

\description{
  Retrieve the number of non-zero elements in the constraint matrix of the
  optimization problem.
}

\usage{
\S4method{getNumNnz}{optObj_clpAPI}(lp)

\S4method{getNumNnz}{optObj_cplexAPI}(lp)

\S4method{getNumNnz}{optObj_glpkAPI}(lp)
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
