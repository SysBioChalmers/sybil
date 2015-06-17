\name{optObj_lpSolveAPI-class}

\Rdversion{1.1}
\encoding{utf8}
\docType{class}

\alias{return_codeLPSOLVE}
\alias{loadMatrixPerColumnLPSOLVE}

\alias{optObj_lpSolveAPI-class}

\title{Class \code{"optObj_lpSolveAPI"}}

\description{
  Structure of the class \code{"optObj_lpSolveAPI"}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the constructor function \code{optObj}:

  \code{test <- optObj(solver = "lpSolveAPI")}.
}

\section{Slots}{
  \describe{
    \item{\code{oobj}:}{
      Object of class \code{"pointerToProb"} containing a pointer to a
      \pkg{lpSolveAPI} problem object.
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

\section{Further usefull Functions}{
  \describe{
    \item{\code{return_codeLPSOLVE}:}{(\code{code})
      prints a human readable translation of return codes of lpSolveAPI.
    }
    \item{\code{loadMatrixPerColumnLPSOLVE}:}{(\code{lpmod, constMat})
      load a constraint matrix (an object of class \code{\link{Matrix}}) to a
      \pkg{lpSolveAPI} problem object column by column.
    }
  }
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  Superclass \code{\linkS4class{optObj}} and constructor function
  \code{\link{optObj}}
}

\examples{
  showClass("optObj_lpSolveAPI")
}

\keyword{classes}


