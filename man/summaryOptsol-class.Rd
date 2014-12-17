\name{summaryOptsol-class}

\Rdversion{1.1}
\encoding{utf8}

\docType{class}

\alias{summaryOptsol-class}

\alias{ex_met,summaryOptsol-method}
\alias{ex_val,summaryOptsol-method}
\alias{image,summaryOptsol-method}
\alias{mod_id,summaryOptsol-method}
\alias{mod_id<-,summaryOptsol-method}
\alias{mod_key,summaryOptsol-method}
\alias{mod_key<-,summaryOptsol-method}
\alias{mod_obj,summaryOptsol-method}
\alias{mod_obj<-,summaryOptsol-method}
\alias{nnzero,summaryOptsol-method}
\alias{nzeros,summaryOptsol-method}
\alias{plot,summaryOptsol,missing-method}
\alias{printExchange,summaryOptsol-method}
\alias{ex_met}
\alias{ex_val}
\alias{mod_obj<-}
\alias{nzeros}
\alias{printExchange}

\title{Class \code{"summaryOptsol"}}

\description{
  Class \code{summaryOptsol} stores a summary of instances of class
  \code{\linkS4class{optsol}}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{summaryOptsol(opt, mod)}.
}

\section{Slots}{
  \describe{
    \item{\code{mod_id}:}{Object of class \code{"character"}
      containing the model id of the analyzed model.
    }
    \item{\code{mod_key}:}{Object of class \code{"character"}
      containing the model key of the used model.
    }
    \item{\code{nzeros}:}{Object of class \code{"integer"}
      giving the number of zeros in the flux distribution.
    }
    \item{\code{nnonzero}:}{Object of class \code{"integer"}
      giving the number of non-zeros in the flux distribution.
    }
    \item{\code{mod_obj}:}{Object of class \code{"numeric"}
      containing the objective coefficients of the model.
    }
    \item{\code{ex_met}:}{Object of class \code{"character"}
      containing the id's of exchange metabolites. These are metabolites which
      are transported across the system boundary.
    }
    \item{\code{ex_val}:}{Object of class \code{"Matrix"}
      with each column being the flux distribution of the exchange metabolites
      of one optimization.
    }
    \item{\code{react_id}:}{Object of class \code{"list"}
      with each list element containing a set of reaction id's limiting one
      optimization. A reactions is considered as \dQuote{limiting}, if it
      has a non-zero flux value and if its flux value hits an upper or lower
      bound.
    }
    \item{\code{chksol}:}{Object of class \code{"checksol"}
      describing return values of the mathematical programming software and
      solution status.
    }
  }
}

\section{Methods}{
  \describe{
    \item{ex_met}{\code{signature(object = "summaryOptsol")}:
      gets the \code{ex_met} slot.
    }
    \item{ex_val}{\code{signature(object = "summaryOptsol")}:
      gets the \code{ex_val} slot.
    }
    \item{\code{plot}:}{\code{signature(x = "summaryOptsol")}:
      plots a \code{\link{histogram}} of the values of the objective function
      in optimal state. Additional arguments can be passed to
      \code{\link{histogram}} via the \code{\dots} argument.
    }
    \item{image}{\code{signature(x = "summaryOptsol")}:
      plots a grey-scale representation of the exchange fluxes of the flux
      distribution. Black: metabolite is produced, grey: metabolite is imported.
      Further arguments are:
      \describe{
        \item{\code{printOut}}{
          A single logical value. If set to \code{FALSE}, a
          \code{\link[lattice]{trellis.object}} is returned invisibly.
          Otherwise, a plot is drawn additionally.\cr
          Default: \code{TRUE}.
        }
        \item{\code{...}}{
          Further arguments to \code{\link[Matrix]{image-methods}}.
        }
      }
    }
    \item{mod_id}{\code{signature(object = "summaryOptsol")}:
      gets the \code{mod_id} slot.
    }
    \item{mod_id<-}{\code{signature(object = "summaryOptsol")}:
      sets the \code{mod_id} slot.
    }
    \item{mod_key}{\code{signature(object = "summaryOptsol")}:
      gets the \code{mod_key} slot.
    }
    \item{mod_key<-}{\code{signature(object = "summaryOptsol")}:
      sets the \code{mod_key} slot.
    }
    \item{mod_obj}{\code{signature(object = "summaryOptsol")}:
      gets the \code{mod_obj} slot.
    }
    \item{mod_obj<-}{\code{signature(object = "summaryOptsol")}:
      sets the \code{mod_obj} slot.
    }
    \item{nnzero}{\code{signature(object = "summaryOptsol")}:
      gets the \code{nnonzero} slot.
    }
    \item{nzeros}{\code{signature(object = "summaryOptsol")}:
      gets the \code{nzeros} slot.
    }
    \item{printExchange}{\code{signature(object = "summaryOptsol")}:
      prints a matrix indicating wether a particular metabolite is taken up or
      produced by the metabolic network given certain conditions. Each line
      corresponds to one metabolite and each column to one optimization.
      A \code{"-"} indicates uptake and \code{"+"} indicates excretion. A
      whitespace character \code{" "} is used, if the metabolite is unused.
      Further arguments are:
      \describe{
        \item{\code{i}}{
          A numeric vector indicating the metabolites (rows) to print:
          \code{i[x]} points to matabolite \code{ec_met(object)[x]}.
        }
        \item{\code{j}}{
          A numeric vector indicating the optimizations (columns) to print.
        }
        \item{\code{dense}}{
          A single Boolean value. If set to \code{TRUE}, each column has a
          column with of one letter.
        }
      }
    }
  }
}

\author{
  Gabriel Gelius-Dietrich
}

\seealso{
  Constructor function \code{\link{summaryOptsol}},
  class \code{\linkS4class{optsol}} and class \code{\linkS4class{modelorg}}.
}

\examples{
  showClass("summaryOptsol")
}

\keyword{classes}
