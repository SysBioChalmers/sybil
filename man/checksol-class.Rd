\name{checksol-class}

\docType{class}

\alias{checksol-class}
\alias{checksol}

\alias{exit_code<-,checksol-method}
\alias{exit_code,checksol-method}
\alias{exit_code<-}
\alias{exit_code}
\alias{exit_meaning<-,checksol-method}
\alias{exit_meaning,checksol-method}
\alias{exit_meaning<-}
\alias{exit_meaning}
\alias{exit_num<-,checksol-method}
\alias{exit_num,checksol-method}
\alias{exit_num<-}
\alias{exit_num}
\alias{num_of_prob<-,checksol-method}
\alias{num_of_prob,checksol-method}
\alias{show,checksol-method}
\alias{status_code<-,checksol-method}
\alias{status_code,checksol-method}
\alias{status_code<-}
\alias{status_code}
\alias{status_meaning<-,checksol-method}
\alias{status_meaning,checksol-method}
\alias{status_meaning<-}
\alias{status_meaning}
\alias{status_num<-,checksol-method}
\alias{status_num,checksol-method}
\alias{status_num<-}
\alias{status_num}

\encoding{utf8}

\title{Structure of the Class "checksol"}

\description{
  Structure of the class \code{"checksol"}. Objects of that class are
  returned by the function \code{\link{checkOptSol}}.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("checksol")}.
}

\section{Slots}{
	 \describe{
    \item{\code{exit_code}:}{Object of class \code{"integer"}
      containing the exit code of the lp solver.
    }
    \item{\code{exit_num}:}{Object of class \code{"integer"}
      containing the number of appearance of a specific exit code.
    }
    \item{\code{exit_meaning}:}{Object of class \code{"character"}
      containing the meaning of the exit code.
    }
    \item{\code{num_of_prob}:}{Object of class \code{"integer"}
      indicating the number of optimization problems.
    }
    \item{\code{status_code}:}{Object of class \code{"integer"}
      containing the solution status of the lp problem.
    }
    \item{\code{status_num}:}{Object of class \code{"integer"}
      containing the number of appearance of a specific solution status.
    }
    \item{\code{status_meaning}:}{Object of class \code{"character"}
      containing the meaning of the solution status.
    }
  }
}

\section{Methods}{
  \describe{
    \item{\code{exit_code<-}:}{\code{signature(object = "checksol")}:
      sets the \code{exit_code} slot.
    }
    \item{\code{exit_code}:}{\code{signature(object = "checksol")}:
      gets the \code{exit_code} slot.
    }
    \item{\code{exit_meaning<-}:}{\code{signature(object = "checksol")}:
      sets the \code{exit_meaning} slot.
    }
    \item{\code{exit_meaning}:}{\code{signature(object = "checksol")}:
      gets the \code{exit_meaning} slot.
    }
    \item{\code{exit_num<-}:}{\code{signature(object = "checksol")}:
      sets the \code{exit_num} slot.
    }
    \item{\code{exit_num}:}{\code{signature(object = "checksol")}:
      gets the \code{exit_num} slot.
    }
    \item{\code{num_of_prob<-}:}{\code{signature(object = "optsol")}:
      sets the \code{num_of_prob} slot.
    }
    \item{\code{num_of_prob}:}{\code{signature(object = "optsol")}:
      gets the \code{num_of_prob} slot.
    }
    \item{\code{show}:}{\code{signature(object = "checksol")}:
      prints some details specific to the instance of class \code{checksol}.
    }
    \item{\code{status_code<-}:}{\code{signature(object = "checksol")}:
      sets the \code{status_code} slot.
    }
    \item{\code{status_code}:}{\code{signature(object = "checksol")}:
      gets the \code{status_code} slot.
    }
    \item{\code{status_meaning<-}:}{\code{signature(object = "checksol")}:
      sets the \code{status_meaning} slot.
    }
    \item{\code{status_meaning}:}{\code{signature(object = "checksol")}:
      gets the \code{status_meaning} slot.
    }
    \item{\code{status_num<-}:}{\code{signature(object = "checksol")}:
      sets the \code{status_num} slot.
    }
    \item{\code{status_num}:}{\code{signature(object = "checksol")}:
      gets the \code{status_num} slot.
    }
  }
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{\code{\link{checkOptSol}}}

\examples{
  showClass("checksol")
}

\keyword{classes}
