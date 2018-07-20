\name{applyChanges-methods}

\docType{methods}
\encoding{utf8}

\alias{applyChanges}
\alias{applyChanges-methods}
\alias{applyChanges,sysBiolAlg-method}
\alias{applyChanges,sysBiolAlg_room-method}

\title{Generic Function to Apply Changes to Objects of Class sysBiolAlg}

\description{
  Use method \code{applyChanges} to apply changes in objects of class
  \code{\linkS4class{sysBiolAlg}}. Changes can be coefficients of the objective
  function, variable bounds or the optimization direction.
}

\usage{
\S4method{applyChanges}{sysBiolAlg}(object, del, obj, ld,
             react    = NULL,
             lb       = NULL,
             ub       = NULL,
             obj_coef = NULL,
             fldind   = TRUE,
             lpdir    = NULL)

\S4method{applyChanges}{sysBiolAlg_room}(object, del, obj, ld,
             react    = NULL,
             lb       = NULL,
             ub       = NULL,
             obj_coef = NULL,
             fldind   = TRUE,
             lpdir    = NULL)
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
  \item{del}{
    A logical value indicating whether variable bounds should be altered or not.
  }
  \item{obj}{
    A logical value indicating whether objective coefficients should be altered
    or not.
  }
  \item{ld}{
    A logical value indicating whether the direction of optimization should be
    altered or not.
  }
  \item{react}{
    A numeric vector containing indices to reactions which should be changed
    (in terms of variable bounds or objective coefficients).\cr
    Default: \code{NULL}.
  }
  \item{lb}{
    Numeric vector of the same length as \code{react}, containing the new lower
    variable bounds.\cr
    Default: \code{NULL}.
  }
  \item{ub}{
    Numeric vector of the same length as \code{react}, containing the new upper
    variable bounds.\cr
    Default: \code{NULL}.
  }
  \item{obj_coef}{
    Numeric vector of the same length as \code{react}, containing the new
    objective coefficients.\cr
    Default: \code{NULL}.
  }
  \item{fldind}{
    Boolean value. If set to \code{TRUE}, (default) indices in \code{"react"} are used
    only for reactions. If set to \code{FALSE}, indices in \code{"react"} are used for
    all variables during optimization, e.g. also for additional variables introduced by
    the \code{\link{mtf}} algorithm. Currently unused by class
    \code{\linkS4class{sysBiolAlg_room}}.\cr
    Default: \code{TRUE}.
  }
  \item{lpdir}{
    A single character value indicating the new direction of optimization.\cr
    Default: \code{NULL}.
  }
}

\value{
  Returns a list containing the original values in order to undo the changes
  with \code{\link{resetChanges}}:
  \item{fi}{
    A numeric vector containing variable id's to apply changes to.
  }
  \item{lb}{
    A numeric vector of the same length as \code{react} containing the original
    variable lower bounds.
  }
  \item{ub}{
    A numeric vector of the same length as \code{react} containing the original
    variable upper bounds.
  }
  \item{obj_coef}{
    A numeric vector of the same length as \code{react} containing the original
    objective coefficients.
  }
  \item{lpdir}{
    A single character value giving the original optimization direction.
  }
  \item{ri}{
    A numeric vector of the same length as \code{react} containing row indices
    of the stoichiometric matrix required to apply changes in variable bounds
    when algorithm \code{"room"} is used.
    (only used by the \code{\linkS4class{sysBiolAlg_room}} method).
  }
  \item{ci}{
    A numeric vector of the same length as \code{react} containing column
    indices of the stoichiometric matrix required to apply changes in variable
    bounds when algorithm \code{"room"} is used.
    (only used by the \code{\linkS4class{sysBiolAlg_room}} method).
  }
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\seealso{
  Class \code{\linkS4class{sysBiolAlg}} and \code{\link{resetChanges}}
}

\keyword{methods}
\keyword{optimize}
