\name{setSolverParm-methods}

\docType{methods}
\encoding{utf8}

\alias{setSolverParm}
\alias{setSolverParm-methods}

\alias{setSolverParm,optObj_clpAPI-method}
\alias{setSolverParm,optObj_cplexAPI-method}
\alias{setSolverParm,optObj_glpkAPI-method}
\alias{setSolverParm,optObj_lpSolveAPI-method}

\title{Set Parameters Used By The Optimization Software}

\description{
  Set parameters used by the optimization software. Parameters are set on a
  key-value basis. Sets of parameters can be set via a named list or a named
  data frame. The names of the parameters itself and possible values differ from
  solver to solver. Please consult the documentation of your solver software
  to get information about available parameters.
}

\usage{
\S4method{setSolverParm}{optObj_clpAPI}(lp, solverParm)

\S4method{setSolverParm}{optObj_cplexAPI}(lp, solverParm)

\S4method{setSolverParm}{optObj_glpkAPI}(lp, solverParm)

\S4method{setSolverParm}{optObj_lpSolveAPI}(lp, solverParm)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI")}}{
      method to use with package \pkg{optObj_clpAPI}. This method is currently
      unused. It is not possible to provide parameters for package \pkg{clpAPI}.
      Always \code{FALSE} will be returned.
    }
    \item{\code{signature(lp = "optObj_cplexAPI")}}{
      method to use with package \pkg{optObj_cplexAPI}. In order to set
      integer parameters (parameters of type \code{CPXINT}), the value must be
      of type integer. For example, like \code{as.integer(42)} or \code{23L}.
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
  \item{solverParm}{
    A named list or data frame containing sets of parameters. They must not
    contain \code{NA} values and every list or data frame element must have
    length one.
  }
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
