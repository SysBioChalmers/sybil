\name{checkVersion-methods}

\docType{methods}
\encoding{utf8}

\alias{checkVersion}
\alias{checkVersion-methods}
\alias{checkVersion,modelorg}
\alias{checkVersion,modelorg-method}

\title{checks Version of modelorg}

\description{
  Checks the Version of the modelorg.
}

\usage{
\S4method{checkVersion}{modelorg}(object)
}

\section{Methods}{
  \describe{
    \item{\code{signature(object = "modelorg")}}{
      method to use with objects of class \code{\linkS4class{modelorg}}.
    }
  }
}

\arguments{
  \item{object}{
    An object of class \code{\linkS4class{modelorg}} or of class
    \code{\linkS4class{summaryOptsol}}.
  }
}

\details{
  This method checks whether this instance of a modelorg-Class is of the currently
  used version. All methods of sybil create the correct version of modelorg, but 
  if objects saved to disk may be of an older version. Current version can be 
  obtained by \code{SYBIL_SETTINGS("VERSION")}.
}

\author{
  Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\value{
  Returns \code{TRUE} if the version is correct. Otherwise returns a character 
  stating the reason.
}

\seealso{
  Class \code{\linkS4class{modelorg}}, 
  method \code{\link{upgradeModelorg}} and \code{\link{SYBIL_SETTINGS}}
}

\keyword{methods}
