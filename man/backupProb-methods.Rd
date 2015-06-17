\name{backupProb-methods}

\docType{methods}
\encoding{utf8}

\alias{backupProb}
\alias{backupProb-methods}

\alias{backupProb,optObj_clpAPI-method}
\alias{backupProb,optObj_cplexAPI-method}
\alias{backupProb,optObj_glpkAPI-method}
\alias{backupProb,optObj_lpSolveAPI-method}

\title{Copies a Problem Object to a New Problem Object}

\description{
  Copies a problem object into a new problem object.
}

\usage{
\S4method{backupProb}{optObj_clpAPI}(lp)

\S4method{backupProb}{optObj_cplexAPI}(lp)

\S4method{backupProb}{optObj_glpkAPI}(lp)

\S4method{backupProb}{optObj_lpSolveAPI}(lp)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI")}}{
      method to use with package \pkg{optObj_clpAPI}.
    }
    \item{\code{signature(lp = "optObj_cplexAPI")}}{
      method to use with package \pkg{optObj_cplexAPI}. The new problem object
      will be in the same CPLEX environment like the original one.
    }
    \item{\code{signature(lp = "optObj_glpkAPI")}}{
      method to use with package \pkg{optObj_glpkAPI}. Building a new problem
      object will reset all parameters to their default. After backing up, set
      all parameters which are not at their default values again.
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
  An object of the same class as given in argument \code{lp} (extending class
  \code{\linkS4class{optObj}}).
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
