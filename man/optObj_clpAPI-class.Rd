\name{optObj_clpAPI-class}

\Rdversion{1.1}
\encoding{utf8}
\docType{class}

\alias{optObj_clpAPI-class}

\title{Class \code{"optObj_clpAPI"}}

\description{
  Structure of the class \code{"optObj_clpAPI"}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the constructor function \code{optObj}:

  \code{test <- optObj(solver = "clpAPI")}.
}

\section{Slots}{
  \describe{
    \item{\code{oobj}:}{
      Object of class \code{"pointerToProb"} containing a pointer to a
      \pkg{clpAPI} problem object.
    }
    \item{\code{solver}:}{
      Object of class \code{"character"} containing the name of the solver
      software (see \code{\link{SYBIL_SETTINGS}} for suitable values).
    }
    \item{\code{method}:}{
      Object of class \code{"character"} containing the method (algorithm) used
      by the solver software (see \code{\link{SYBIL_SETTINGS}} for suitable
      values).
    }
    \item{\code{probType}:}{
      Object of class \code{"character"} giving the problem type (see
      \code{\link{optObj}} for suitable values).
    }
  }
}

\section{Extends}{
  Class \code{"\linkS4class{optObj}"}, directly.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  Superclass \code{\linkS4class{optObj}} and constructor function
  \code{\link{optObj}}
}

\examples{
  showClass("optObj_clpAPI")
}

\keyword{classes}


