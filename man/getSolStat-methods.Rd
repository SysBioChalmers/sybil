\name{getSolStat-methods}

\docType{methods}
\encoding{utf8}

\alias{getSolStat}
\alias{getSolStat-methods}

\alias{getSolStat,optObj_clpAPI-method}
\alias{getSolStat,optObj_cplexAPI-method}
\alias{getSolStat,optObj_glpkAPI-method}
\alias{getSolStat,optObj_lpSolveAPI-method}

\title{Get Solution Status After Optimization}

\description{
  Get solution status after optimization.
}

\usage{
\S4method{getSolStat}{optObj_clpAPI}(lp)

\S4method{getSolStat}{optObj_cplexAPI}(lp)

\S4method{getSolStat}{optObj_glpkAPI}(lp)

\S4method{getSolStat}{optObj_lpSolveAPI}(lp)
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
      method to use with package \pkg{optObj_lpSolveAPI}. This method returns
      \code{NA}. Package \pkg{lpSolveAPI} does not provide a solution status.
    }
  }
}

\arguments{
  \item{lp}{
    An object extending class \code{\linkS4class{optObj}}.
  }
}

\value{
  Returns a single numeric value indicating the solution status after
  optimization.
}

\author{
  Gabriel Gelius-Dietrich
}

\seealso{
  Function \code{\link{getMeanStatus}} and superclass
  \code{\linkS4class{optObj}} and constructor function
  \code{\link{optObj}}.
}

\keyword{methods}
\keyword{optimize}
