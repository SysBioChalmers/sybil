\name{setObjDir-methods}

\docType{methods}
\encoding{utf8}

\alias{setObjDir}
\alias{setObjDir-methods}

\alias{setObjDir,optObj_clpAPI,character-method}
\alias{setObjDir,optObj_clpAPI,numeric-method}
\alias{setObjDir,optObj_cplexAPI,character-method}
\alias{setObjDir,optObj_cplexAPI,integer-method}
\alias{setObjDir,optObj_cplexAPI,numeric-method}
\alias{setObjDir,optObj_glpkAPI,character-method}
\alias{setObjDir,optObj_glpkAPI,integer-method}
\alias{setObjDir,optObj_glpkAPI,numeric-method}
\alias{setObjDir,optObj_lpSolveAPI,character-method}
\alias{setObjDir,optObj_lpSolveAPI,numeric-method}

\title{Set Direction of Optimization}

\description{
  Set direction of optimization.
}

\usage{
\S4method{setObjDir}{optObj_clpAPI,character}(lp, lpdir)

\S4method{setObjDir}{optObj_clpAPI,numeric}(lp, lpdir)

\S4method{setObjDir}{optObj_cplexAPI,character}(lp, lpdir)

\S4method{setObjDir}{optObj_cplexAPI,integer}(lp, lpdir)

\S4method{setObjDir}{optObj_cplexAPI,numeric}(lp, lpdir)

\S4method{setObjDir}{optObj_glpkAPI,character}(lp, lpdir)

\S4method{setObjDir}{optObj_glpkAPI,integer}(lp, lpdir)

\S4method{setObjDir}{optObj_glpkAPI,numeric}(lp, lpdir)

\S4method{setObjDir}{optObj_lpSolveAPI,character}(lp, lpdir)

\S4method{setObjDir}{optObj_lpSolveAPI,numeric}(lp, lpdir)
}

\section{Methods}{
  \describe{
    \item{\code{signature(lp = "optObj_clpAPI", lpdir = "character")}}{
      method to use with package \pkg{optObj_clpAPI}. Set \code{lpdir} to
      \code{"max"} for maximization or \code{"min"} for minimization.
    }
    \item{\code{signature(lp = "optObj_clpAPI", lpdir = "numeric")}}{
      method to use with package \pkg{optObj_clpAPI}. Set \code{lpdir} to
      \code{-1} for maximization or \code{1} for minimization.
    }
    \item{\code{signature(lp = "optObj_cplexAPI", lpdir = "character")}}{
      method to use with package \pkg{optObj_cplexAPI}. Set \code{lpdir} to
      \code{"max"} for maximization or \code{"min"} for minimization.
    }
    \item{\code{signature(lp = "optObj_cplexAPI", lpdir = "integer")}}{
      method to use with package \pkg{optObj_cplexAPI}. Set \code{lpdir} to
      \code{CPX_MAX} for maximization or \code{CPX_MIN} for minimization.
    }
    \item{\code{signature(lp = "optObj_cplexAPI", lpdir = "numeric")}}{
      method to use with package \pkg{optObj_cplexAPI}. Set \code{lpdir} to
      \code{-1} for maximization or \code{1} for minimization.
    }
    \item{\code{signature(lp = "optObj_glpkAPI", lpdir = "character")}}{
      method to use with package \pkg{optObj_glpkAPI}. Set \code{lpdir} to
      \code{"max"} for maximization or \code{"min"} for minimization.
    }
    \item{\code{signature(lp = "optObj_glpkAPI", lpdir = "integer")}}{
      method to use with package \pkg{optObj_glpkAPI}. Set \code{lpdir} to
      \code{GLP_MAX} for maximization or \code{GLP_MIN} for minimization.
    }
    \item{\code{signature(lp = "optObj_glpkAPI", lpdir = "numeric")}}{
      method to use with package \pkg{optObj_glpkAPI}. Set \code{lpdir} to
      \code{-1} for maximization or \code{1} for minimization.
    }
    \item{\code{signature(lp = "optObj_lpSolveAPI", lpdir = "character")}}{
      method to use with package \pkg{optObj_lpSolveAPI}. Set \code{lpdir} to
      \code{"max"} for maximization or \code{"min"} for minimization.
    }
    \item{\code{signature(lp = "optObj_lpSolveAPI", lpdir = "numeric")}}{
      method to use with package \pkg{optObj_lpSolveAPI}. Set \code{lpdir} to
      \code{-1} for maximization or \code{1} for minimization.
    }
  }
}

\arguments{
  \item{lp}{
    An object extending class \code{\linkS4class{optObj}}.
  }
  \item{lpdir}{
    A single character string, numeric or integer value. Can be set to
    \code{"max"} or \code{-1} for maximization, or \code{"min"} or \code{1} for
    minimization. For packages \pkg{cplexAPI} and \code{glpkAPI} it is also
    possible to use the corresponding constant given by the package.
  }
}

\author{
  Gabriel Gelius-Dietrich
}

\seealso{
  Superclass \code{\linkS4class{optObj}} and constructor function
  \code{\link{optObj}}.
}

\keyword{methods}
\keyword{optimize}
