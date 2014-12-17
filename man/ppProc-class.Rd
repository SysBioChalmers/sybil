\name{ppProc-class}

\Rdversion{1.1}
\docType{class}

\alias{ppProc-class}
\alias{ppProc}

\alias{cmd,ppProc-method}
\alias{cmd<-,ppProc-method}
\alias{cmd}
\alias{cmd<-}
\alias{ind,ppProc-method}
\alias{ind<-,ppProc-method}
\alias{ind}
\alias{ind<-}
\alias{pa,ppProc-method}
\alias{pa<-,ppProc-method}
\alias{pa}
\alias{pa<-}

\title{Class \code{"ppProc"}}

\description{
  Structure of the class \code{"ppProc"}. Objects of that class are
  returned as part of class \code{\linkS4class{optsol}} when performing
  pre- or post-processing of an optimization, e.g. in
  \code{\link{optimizeProb}}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the function \code{ppProc}:

  \code{test <- ppProc(cmd)}.
  \describe{
    \item{\code{cmd}:}{Object of class \code{"list"}.
    }
  }
}

\section{Slots}{
  \describe{
    \item{\code{cmd}:}{Object of class \code{"list"}
      a character vector or a list of character strings containing pre- or
      postprocessing commands.
    }
    \item{\code{pa}:}{Object of class \code{"list"}
      return values of the pre- or postprocessing commands. They can be
      numeric, integer, character, list or of class
      \code{\linkS4class{sybilError}}.
    }
    \item{\code{ind}:}{Object of class \code{"integer"}
      giving the indices of the optimizations when pre- or postprocessing was
      performed.
    }
  }
}

\section{Methods}{
  \describe{
    \item{\code{cmd}:}{\code{signature(object = "ppProc")}:
      gets the \code{cmd} slot.
    }
    \item{\code{cmd<-}:}{\code{signature(object = "ppProc")}:
      sets the \code{cmd} slot.
    }
    \item{\code{pa}:}{\code{signature(object = "ppProc")}:
      gets the \code{pa} slot.
    }
    \item{\code{pa<-}:}{\code{signature(object = "ppProc")}:
      sets the \code{pa} slot.
    }
    \item{\code{ind}:}{\code{signature(object = "ppProc")}:
      gets the \code{ind} slot.
    }
    \item{\code{ind<-}:}{\code{signature(object = "ppProc")}:
      sets the \code{ind} slot.
    }
  }
}

\author{Gabriel Gelius-Dietrich}

\seealso{
  \code{\link{optimizeProb}} and \code{\link{optimizer}}
}
      
\examples{
  showClass("ppProc")
}

\keyword{classes}
