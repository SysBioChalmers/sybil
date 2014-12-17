\name{resetChanges-methods}

\docType{methods}
\encoding{utf8}

\alias{resetChanges}
\alias{resetChanges-methods}
\alias{resetChanges,sysBiolAlg-method}
\alias{resetChanges,sysBiolAlg_room-method}

\title{Generic Function to Reset Temporary Changes in Objects of Class sysBiolAlg}

\description{
  Use method \code{resetChanges} to undo changes in objects of class
  \code{\linkS4class{sysBiolAlg}} made by \code{\link{applyChanges}}.
}

\usage{
\S4method{resetChanges}{sysBiolAlg}(object, old_val)

\S4method{resetChanges}{sysBiolAlg_room}(object, old_val)
}

\section{Methods}{
  \describe{
    \item{\code{signature(object = "sysBiolAlg")}}{
      Method used with objects extending class \code{\linkS4class{sysBiolAlg}}
    }
    \item{\code{signature(object = "sysBiolAlg_room")}}{
      Method used with objects of class \code{\linkS4class{sysBiolAlg_room}}
    }
  }
}

\arguments{
  \item{object}{
    An object of class \code{\linkS4class{sysBiolAlg}}.
  }
  \item{old_val}{
    A list containing the original values of the model. This list is returned
    by \code{\link{applyChanges}}.
  }
}

\value{
  Invisibly \code{TRUE} will be returned.
}

\author{
  Gabriel Gelius-Dietrich
}

\seealso{
  Class \code{\linkS4class{sysBiolAlg}} and \code{\link{applyChanges}}
}

\keyword{methods}
\keyword{optimize}
