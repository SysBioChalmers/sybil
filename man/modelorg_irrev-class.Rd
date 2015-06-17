\name{modelorg_irrev-class}

\docType{class}

\alias{modelorg_irrev-class}
\alias{modelorg_irrev}
\alias{matchrev<-,modelorg_irrev-method}
\alias{matchrev,modelorg_irrev-method}
\alias{matchrev<-}
\alias{matchrev}
\alias{irrev<-,modelorg_irrev-method}
\alias{irrev,modelorg_irrev-method}
\alias{irrev<-}
\alias{irrev}
\alias{rev2irrev<-,modelorg_irrev-method}
\alias{rev2irrev,modelorg_irrev-method}
\alias{rev2irrev<-}
\alias{rev2irrev}
\alias{irrev2rev<-,modelorg_irrev-method}
\alias{irrev2rev,modelorg_irrev-method}
\alias{irrev2rev<-}
\alias{irrev2rev}

\encoding{utf8}

\title{Class for Metabolic Networks in Irreversible Format.}

\description{
  Structure of the class \code{"modelorg_irrev"}. Objects of that class are
  returned by the function \code{\link{mod2irrev}}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the function \code{modelorg_irrev}:

  \code{test <- modelorg_irrev(id = "foo", name = "bar")}.
}

\section{Slots}{
  \describe{
    \item{\code{irrev}:}{
      Object of class \code{"logical"} indicating if the model is in
      irreversible format.
    }
    \item{\code{matchrev}:}{
      Object of class \code{"integer"} matching of forward and backward
      reactions of a reversible reaction.
    }
    \item{\code{rev2irrev}:}{
      Object of class \code{"matrix"} containing the reaction id's of the
      corresponding reactions in irreversible format.
    }
    \item{\code{irrev2rev}:}{
      Object of class \code{"integer"} containing the reaction id's of the
      corresponding reaction in reversible format.
    }
  }
}

\section{Extends}{
  Class \code{"\linkS4class{modelorg}"}, directly.
}

\section{Methods}{
  \describe{
    \item{\code{irrev<-}:}{
      \code{signature(object = "modelorg_irrev")}: sets the \code{irrev} slot.
    }
    \item{\code{irrev}:}{
      \code{signature(object = "modelorg_irrev")}: gets the \code{irrev} slot.
    }
    \item{\code{matchrev<-}:}{
      \code{signature(object = "modelorg_irrev")}: sets the \code{matchrev}
      slot.
    }
    \item{\code{matchrev}:}{
      \code{signature(object = "modelorg_irrev")}: gets the \code{matchrev}
      slot.
    }
    \item{\code{rev2irrev<-}:}{
      \code{signature(object = "modelorg_irrev")}: sets the \code{rev2irrev}
      slot.
    }
    \item{\code{rev2irrev}:}{
      \code{signature(object = "modelorg_irrev")}: gets the \code{rev2irrev}
      slot.
    }
    \item{\code{irrev2rev<-}:}{
      \code{signature(object = "modelorg_irrev")}: sets the \code{irrev2rev}
      slot.
    }
    \item{\code{irrev2rev}:}{
      \code{signature(object = "modelorg_irrev")}: gets the \code{irrev2rev}
      slot.
    }
  }
}
      
\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  \code{\linkS4class{modelorg}}
}

\examples{
  showClass("modelorg_irrev")
}

\keyword{classes}
