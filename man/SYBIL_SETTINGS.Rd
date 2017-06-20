\name{SYBIL_SETTINGS}
\alias{SYBIL_SETTINGS}
\alias{SOLVER}
\alias{METHOD}
\alias{TOLERANCE}
\alias{MAXIMUM}
\alias{ALGORITHM}
\alias{OPT_DIRECTION}
\alias{USE_NAMES}
\alias{PATH_TO_MODEL}
\alias{SOLVER_CTRL_PARM}

\encoding{utf8}

\title{
  Set and Get sybil Parameters
}

\description{
  Manage a set of default parameter settings for sybil.
}

\usage{
  SYBIL_SETTINGS(parm, value, ...)
}

\arguments{
  \item{parm}{
    A character string giving the name of the parameter to set.
  }
  \item{value}{
    The corresponding value.
  }
  \item{...}{
    Further arguments passed to \code{\link{checkDefaultMethod}}. Only used if
    parameters \code{"SOLVER"} or \code{"METHOD"} are set.
  }
}

\details{
  Typical usages are
  \preformatted{
    SYBIL_SETTINGS(parm, value)
    SYBIL_SETTINGS(parm)
    SYBIL_SETTINGS()
  }
  Possible parameters are:
  \describe{
    \item{\code{"SOLVER"}}{
      The default solver for lp problems. Possible values are depend on your
      installed API package.\cr
      \describe{
          \item{\pkg{glpkAPI}:}{
            \code{"glpkAPI"},
          }
          \item{\pkg{cplexAPI}:}{
            \code{"cplexAPI"},
          }
          \item{\pkg{clpAPI}:}{
            \code{"clpAPI"},
          }
          \item{\pkg{lpSolveAPI}:}{
            \code{"lpSolveAPI"}.
          }
      }
      Default: \code{"glpkAPI"}.
    }
    \item{\code{"METHOD"}}{
      The default method to solve lp problems. Possible values are
      \describe{
          \item{\pkg{glpkAPI}:}{
            \code{"simplex"}, \code{"interior"}, \code{"exact"} or \code{mip}.
          }
          \item{\pkg{cplexAPI}:}{
            \code{"lpopt"}, \code{"primopt"} \code{"dualopt"}, \code{"baropt"},
            \code{"hybbaropt"}, \code{"hybnetopt"}, \code{"siftopt"},
            \code{mipopt} or \code{qpopt}.
          }
          \item{\pkg{clpAPI}:}{
            \code{"general_solve"}, \code{"inidual"} \code{"iniprimal"},
            \code{"inibarrier"}, \code{"inibarriernoc"}, \code{"idiot"},
            \code{"dual"} or \code{"primal"}.
          }
          \item{\pkg{lpSolveAPI}:}{
            \code{"lp_solve"}.
          }
      }
      Default: \code{"simplex"}.\cr
      If the parameter \code{"SOLVER"} is changed, the corrsponding default
      \code{"METHOD"} is the first one mentioned, e.g. for \code{"cplexAPI"},
      it will be \code{"lpopt"}. This change is done automatically when changing
      the solver. It is not possible, to set a not existing \code{"METHOD"}
      for a particular \code{"SOLVER"}, the corresponding default value will
      be used in such a case.
    }
    \item{\code{"MAXIMUM"}}{
      Absolute maximum value.\cr
      Default: \code{1000}.
    }
    \item{\code{"MODELORG_VERSION"}}{
      Currtent version of \code{modelorg}-Class.\cr
      Value: \code{"2.0"}.\cr
      This value must not be changed.
    }
    \item{\code{"ALGORITHM"}}{
      Algorithm to use in order to analyze metabolic networks.
      Possible values are:
      \describe{
        \item{\code{"\link[=sysBiolAlg_fba-class]{fba}"}}{
          flux-balance analysis,
        }
        \item{\code{"\link[=sysBiolAlg_fv-class]{fv}"}}{
          flux-variance analysis,
        }
        \item{\code{"\link[=sysBiolAlg_mtf-class]{mtf}"}}{
          minimize total flux,
        }
        \item{\code{"\link[=sysBiolAlg_moma-class]{moma}"}}{
          minimization of metabolic adjustment (MOMA),
        }
        \item{\code{"\link[=sysBiolAlg_lmoma-class]{lmoma}"}}{
          linear version of MOMA,
        }
        \item{\code{"\link[=sysBiolAlg_room-class]{room}"}}{
          regulatory on/off minimization (ROOM).
        }
      }
      Default: \code{"fba"}.
    }
    \item{\code{"OPT_DIRECTION"}}{
      Direction of optimization. Can be \code{"max"} or \code{"min"}.\cr
      Default: \code{"max"}.
    }
    \item{\code{"USE_NAMES"}}{
      A logical value indicating if reaction id's and metabolite id's (or other
      names) should be used as names for variables and constraints in objects of
      class \code{\linkS4class{sysBiolAlg}}.\cr
      Default: \code{FALSE}.
    }
    \item{\code{"PATH_TO_MODEL"}}{
      Path to a directory to read or write files.\cr
      Default: \code{"."}.
    }
    \item{\code{"SOLVER_CTRL_PARM"}}{
      A \code{data.frame} giving parameters to the optimizer software
      (e.g. GLPK).\cr
      Default: \code{as.data.frame(NA)}.
    }
    \item{\code{"TOLERANCE"}}{
      Tolerance value.\cr
      Default: \code{1E-6}.
    }
  }
}

\value{
  If successful, a set of parameters to sybil will be returned.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  \code{\link{checkDefaultMethod}}
}

\examples{
  ## show all current parameters
  SYBIL_SETTINGS()
  
  ## show current setting for "SOLVER"
  SYBIL_SETTINGS("SOLVER")

  ## change current solver to glpkAPI
  SYBIL_SETTINGS("SOLVER", "glpkAPI")
\dontrun{
  ## this needs cplexAPI installed
  ## change current solver to cplexAPI
  SYBIL_SETTINGS("SOLVER", "cplexAPI")
}
}

\keyword{ optimize }
