\name{reactId_Exch-class}

\Rdversion{1.1}

\docType{class}
\encoding{utf8}

\alias{reactId_Exch-class}
\alias{reactId_Exch}
\alias{met_id,reactId_Exch-method}
\alias{met_id<-,reactId_Exch-method}
\alias{met_pos}
\alias{met_pos,reactId_Exch-method}
\alias{met_pos<-}
\alias{met_pos<-,reactId_Exch-method}
\alias{uptake}
\alias{uptake,reactId_Exch-method}
\alias{uptake<-}
\alias{uptake<-,reactId_Exch-method}
\alias{uptMet}
\alias{uptMet,reactId_Exch-method}
\alias{uptReact}
\alias{uptReact,reactId_Exch-method}
\alias{lowbnd,reactId_Exch-method}
\alias{lowbnd<-,reactId_Exch-method}
\alias{uppbnd,reactId_Exch-method}
\alias{uppbnd<-,reactId_Exch-method}
\alias{[,reactId_Exch,ANY,ANY,ANY-method}

\title{Class \code{"reactId_Exch"}}

\description{
  Structure of the class \code{"reactId_Exch"}. Objects of that class are
  returned by the function \code{\link{findExchReact}}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("reactId_Exch", mod_id, mod_key, rpnt, rid, upt, mpnt, mid, lb, ub)}.
  \describe{
    \item{\code{mod_id}:}{
      Object of class \code{"character"} containing the model id.
    }
    \item{\code{mod_key}:}{
      Object of class \code{"character"} containing the model key.
    }
    \item{\code{rpnt}:}{
      Object of class \code{"numeric"} containing the column indices in a
      stoichiometric matrix of the reactions given in \code{rid}.
    }
    \item{\code{rid}:}{
      Object of class \code{"character"} containing the reaction id's
      corresponding to argument \code{rpnt}.
    }
    \item{\code{upt}:}{
      Object of class \code{"logical"}: \code{upt[j]} equals \code{TRUE} if
      reaction \eqn{j} in \code{rid} is an uptake reaction (an exchange reaction
      with a lower bound less than zero).
    }
    \item{\code{mpnt}:}{
      Object of class \code{"numeric"} containing the row indices in a
      stoichiometric matrix of the metabolites given in \code{mid}. The reaction
      given in \code{rid[j]} transports metabolite \code{mid[j]} across the
      system boundary of the model. 
    }
    \item{\code{mid}:}{
      Object of class \code{"character"} containing the metabolite id's
      corresponding to argument \code{mpnt}.
    }
    \item{\code{lb}:}{
      Object of class \code{"numeric"} containing the lower bounds of the
      reactions given in \code{rpnt}.
    }
    \item{\code{ub}:}{
      Object of class \code{"numeric"} containing the upper bounds of the
      reactions given in \code{rpnt}.
    }
  }
}

\section{Slots}{
  \describe{
    \item{\code{uptake}:}{
      Object of class \code{"logical"} indicating if a certain reaction is an
      uptake reaction or not.
    }
    \item{\code{met_pos}:}{
      Object of class \code{"integer"} containing the row indices of metabolite
      id's in the stoichiometric matrix of the metabolic model with id
      \code{mod_id}.
    }
    \item{\code{met_id}:}{
      Object of class \code{"character"} containing the metabolite id's
      corresponding to the indices given in slot \code{met_pos}.
    }
    \item{\code{lowbnd}:}{
      Object of class \code{"numeric"} containing the lower bounds of the
      reactions given in slot \code{react_pos}.
    }
    \item{\code{uppbnd}:}{
      Object of class \code{"numeric"} containing the upper bounds of the
      reactions given in slot \code{react_pos}.
    }
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

\section{Extends}{
  Class \code{"\linkS4class{reactId}"}, directly.
}

\section{Methods}{
  \describe{
    \item{met_pos}{\code{signature(object = "reactId_Exch")}:
      gets the \code{met_pos} slot.
    }
    \item{met_pos<-}{\code{signature(object = "reactId_Exch")}:
      sets the \code{met_pos} slot.
    }
    \item{met_id}{\code{signature(object = "reactId_Exch")}:
      gets the \code{met_id} slot.
    }
    \item{met_id<-}{\code{signature(object = "reactId_Exch")}:
      sets the \code{met_id} slot.
    }
    \item{met_pos}{\code{signature(object = "reactId_Exch")}:
      gets the \code{met_pos} slot.
    }
    \item{met_pos<-}{\code{signature(object = "reactId_Exch")}:
      sets the \code{met_pos} slot.
    }
    \item{lowbnd}{\code{signature(object = "reactId_Exch")}:
      gets the \code{lowbnd} slot.
    }
    \item{lowbnd<-}{\code{signature(object = "reactId_Exch")}:
      sets the \code{lowbnd} slot.
    }
    \item{uppbnd}{\code{signature(object = "reactId_Exch")}:
      gets the \code{uppbnd} slot.
    }
    \item{uppbnd<-}{\code{signature(object = "reactId_Exch")}:
      sets the \code{uppbnd} slot.
    }
    \item{uptake}{\code{signature(object = "reactId_Exch")}:
      gets the \code{uptake} slot.
    }
    \item{uptake<-}{\code{signature(object = "reactId_Exch")}:
      sets the \code{uptake} slot.
    }
    \item{uptReact}{\code{signature(object = "reactId_Exch")}:
      gets the id's of uptake reactions.
    }
    \item{uptMet}{\code{signature(object = "reactId_Exch")}:
      gets the metabolite id's of metabolites used by uptake reactions.
    }
    \item{\code{[}:}{
      \code{signature(x = "reactId_Exch")}: access like a vector.
      \code{x[i]} returns a new object of class \code{reactId_Exch} containing
      the \code{i}th exchange reaction id.
    }
    \item{\code{show}:}{
      \code{signature(x = "reactId_Exch")}: prints a table of all exchange
      reactions. If an upper or lower bound is equal or greater than
      \code{abs(SYBIL_SETINGS("MAXIMUM"))}, it will be shown as \code{Inf} or
      \code{-Inf}.
    }
  }
}

\author{Gabriel Gelius-Dietrich}

\seealso{\code{\link{checkReactId}}}

\examples{
  showClass("reactId")
}

\keyword{classes}

