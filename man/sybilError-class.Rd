\name{sybilError-class}

\Rdversion{1.1}
\docType{class}

\alias{sybilError-class}
\alias{sybilError}

\alias{emsg,sybilError-method}
\alias{emsg<-,sybilError-method}
\alias{emsg}
\alias{emsg<-}
\alias{enum,sybilError-method}
\alias{enum<-,sybilError-method}
\alias{enum}
\alias{enum<-}

\title{Class \code{"sybilError"}}

\description{
  Structure of the class \code{"sybilError"}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the function \code{sybilError}:

  \code{test <- sybilError(errmsg = "", number = NA)}.
  \describe{
    \item{\code{errmsg}:}{Object of class \code{"character"} containing an
      error message.
    }
    \item{\code{number}:}{Object of class \code{"integer"} containing an error
      number.
    }
  }
}

\section{Slots}{
  \describe{
    \item{\code{emsg}:}{Object of class \code{"character"} error message.
    }
    \item{\code{enum}:}{Object of class \code{"integer"} error number.
    }
  }
}

\section{Methods}{
  \describe{
    \item{\code{emsg}:}{\code{signature(object = "sybilError")}:
      gets the \code{emsg} slot.
    }
    \item{\code{emsg<-}:}{\code{signature(object = "sybilError")}:
      sets the \code{emsg} slot.
    }
    \item{\code{enum}:}{\code{signature(object = "sybilError")}:
      gets the \code{enum} slot.
    }
    \item{\code{enum<-}:}{\code{signature(object = "sybilError")}:
      sets the \code{enum} slot.
    }
  }
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  \code{\link{optimizeProb}}
}
      
\examples{
  showClass("sybilError")
}

\keyword{classes}
