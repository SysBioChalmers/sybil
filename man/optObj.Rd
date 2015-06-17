\name{optObj}
\alias{optObj}

\title{
  General Constructor Function For Objects of Class \code{optObj}
}
\description{
  This function serves as a user constructor function for objects of class
  \code{\linkS4class{optObj}}.
}
\usage{
  optObj(solver = SYBIL_SETTINGS("SOLVER"),
         method = SYBIL_SETTINGS("METHOD"),
         pType = "lp", prefix = "optObj", sep = "_")
}

\arguments{
  \item{solver}{
    A single character string giving the name of the solver package to use.
    See \code{\link{SYBIL_SETTINGS}} for possible values.\cr
    Default: \code{SYBIL_SETTINGS("SOLVER")}.
  }
  \item{method}{
    A single character string containing the name of the method used by
    \code{solver}. See \code{\link{SYBIL_SETTINGS}} for possible values. 
    If missing or not available, the default method for \code{solver} is used
    (see also \code{\link{checkDefaultMethod}}).\cr
    Default: \code{SYBIL_SETTINGS("METHOD")}.
  }
  \item{pType}{
    A single character string containing the type of optimization problem. Can
    be \code{"lp"}: linear programming, \code{"mip"}: mixed integer programming
    or \code{"qp"}: quadratic programming.\cr
    Default: \code{"lp"}.
  }
  \item{prefix}{
    A single character string containing a prefix for the new class name.\cr
    Default: \code{"optObj"}.
  }
  \item{sep}{
    A single character string containing a separator for \code{prefix} and
    \code{solver}.\cr
    Default: \code{"_"}.
  }
}

\details{
  If argument \code{solver} is set to \code{"foo"} and \code{prefix} is set
  to \code{"optObj"} (default), \code{optObj} will try to build an instance of
  class \code{optObj_foo}. If \code{solver} does not contain a valid name of a
  solver package (this is checked by \code{\link{checkDefaultMethod}}), the
  default solver package will be used (see \code{\link{SYBIL_SETTINGS}}).
  For the name of the class, the arguments \code{prefix} and \code{solver} are
  stick together separated by \code{sep} (default: a single underscore
  \code{"_"}): \code{prefix_solver}.
}

\value{
  An instance of a subclass of class \code{\linkS4class{optObj}}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  Class \code{\linkS4class{optObj}}, \code{\link{SYBIL_SETTINGS}}
  and \code{\link{checkDefaultMethod}}.
}

\keyword{classes}
