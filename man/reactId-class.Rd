\name{reactId-class}

\docType{class}
\encoding{utf8}

\alias{reactId-class}
\alias{reactId}
\alias{react_pos<-,reactId-method}
\alias{react_pos,reactId-method}
\alias{react_pos<-}
\alias{react_pos}
\alias{react_id<-,reactId-method}
\alias{react_id,reactId-method}
\alias{mod_id,reactId-method}
\alias{mod_id<-,reactId-method}
\alias{mod_key,reactId-method}
\alias{mod_key<-,reactId-method}
\alias{length,reactId-method}
\alias{[,reactId,ANY,ANY,ANY-method}

\title{Structure of Class "reactId"}

\description{
  Structure of the class \code{"reactId"}. Objects of that class are
  returned by the function \code{\link{checkReactId}}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("reactId", mod_id, pnt, id = NULL, mod_key = "")}.
  \describe{
    \item{\code{mod_id}:}{
      Object of class \code{"character"} containing the model id.
    }
    \item{\code{pnt}:}{
      Object of class \code{"numeric"} containing the column indices in a
      stoichiometric matrix of the reactions given in \code{react}.
    }
    \item{\code{id}:}{
      Object of class \code{"character"} containing the reaction id's
      corresponding to argument \code{pos}. If set to \code{NULL} (default),
      no reaction id's are used.
    }
    \item{\code{mod_key}:}{
      Object of class \code{"character"} containing the model key.
    }
  }
}

\section{Slots}{
  \describe{
    \item{\code{mod_id}:}{
      Object of class \code{"character"} containing the model id.
    }
    \item{\code{mod_key}:}{Object of class \code{"character"}
      containing the model key of the used model.
    }
    \item{\code{react_pos}:}{
      Object of class \code{"integer"} containing the column indices of reaction
      id's in the stoichiometric matrix of the metabolic model with id
      \code{mod_id}.
    }
    \item{\code{react_id}:}{
      Object of class \code{"character"} containing the reaction id's
      corresponding to the indices given in slot \code{react_pos}.
    }
    \item{\code{react_num}:}{
      Object of class \code{"integer"} containing the number of reaction id's.
    }
  }
}

\section{Methods}{
  \describe{
    \item{\code{mod_id<-}:}{
      \code{signature(object = "reactId")}: sets the \code{mod_id} slot.
    }
    \item{\code{mod_id}:}{
      \code{signature(object = "reactId")}: gets the \code{mod_id} slot.
    }
    \item{\code{mod_key<-}:}{
      \code{signature(object = "reactId")}: sets the \code{mod_key} slot.
    }
    \item{\code{mod_key}:}{
      \code{signature(object = "reactId")}: gets the \code{mod_key} slot.
    }
    \item{\code{react_pos<-}:}{
      \code{signature(object = "reactId")}: sets the \code{react_pos} slot.
    }
    \item{\code{react_pos}:}{
      \code{signature(object = "reactId")}: gets the \code{react_pos} slot.
    }
    \item{\code{react_id<-}:}{
      \code{signature(object = "reactId")}: sets the \code{react_id} slot.
    }
    \item{\code{react_id}:}{
      \code{signature(object = "reactId")}: gets the \code{react_id} slot.
    }
    \item{\code{length}}{
      \code{signature(object = "reactId")}: returns the number of reaction id's.
    }
    \item{\code{[}:}{
      \code{signature(x = "reactId")}: access like a vector.
      \code{x[i]} returns a new object of class \code{reactId} containing
      the \code{i}th reaction id.
    }
  }
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{\code{\link{checkReactId}}}

\examples{
  showClass("reactId")
}

\keyword{classes}

