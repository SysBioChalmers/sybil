\name{printMetabolite-methods}

\docType{methods}
\encoding{utf8}

\alias{printMetabolite}
\alias{printMetabolite-methods}
\alias{printMetabolite,modelorg-method}

\title{Print Rows of the Stoichiometric Matrix}

\description{
  Print the rows of the stoichiometric matrix or an FBA model in CPLEX LP
  file format.
}

\usage{
\S4method{printMetabolite}{modelorg}(object, met, FBAlp = FALSE, printOut = TRUE, ...)
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
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{met}{
    A numeric or character vector containing the metabolite id's of metabolites
    to print out. If missing, all metabolites given in the model are used.
  }
  \item{FBAlp}{
    A single logical value. If set to \code{TRUE}, the output will be in
    CPLEX LP file format, including the objective function given in the model
    and reaction bounds. Additionally, if set to \code{TRUE}, argument
    \code{met} will be ignored; all metabolites present in the model are used.
    See also Details.\cr
    Default: \code{FALSE}.
  }
  \item{printOut}{
    A single Boolean value. If set to \code{TRUE}, the desired reactions will
    be printed via the \code{\link{cat}} function.\cr
    Default: \code{TRUE}.
  }
  \item{...}{
    Further arguments passed to \code{\link{cat}}, e.g. argument \code{file}.
  }
}

\details{
  Metabolite id's beginning with a digit or period will be prefixed by the
  letter \code{"r"}, reaction id's beginning with a digit or period will be
  prefixed by the letter \code{"x"} and square brackets in reaction or
  metabolite id's will be replaced by round brackets.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Mayo Roettger <mayo.roettger@hhu.de>
}


\value{
  The \code{\linkS4class{modelorg}} method returns a character vector of length
  equal to the number of metabolites given in argument \code{met}, invisibly.
  Each string represents the reaction participation of one particular
  metabolite.
}

\seealso{
  Class \code{\linkS4class{modelorg}}
}

\keyword{methods}
