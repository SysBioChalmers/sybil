\name{printReaction-methods}

\docType{methods}
\encoding{utf8}

\alias{printReaction}
\alias{printReaction-methods}
\alias{printReaction,modelorg,ANY-method}
\alias{printReaction,summaryOptsol,modelorg-method}

\title{Print Columns of the Stoichiometric Matrix}

\description{
  Print the columns of the stoichiometric matrix.
}

\usage{
\S4method{printReaction}{modelorg,ANY}(object, react, printOut = TRUE, ...)
\S4method{printReaction}{summaryOptsol,modelorg}(object, mod, j, ...)
}

\section{Methods}{
  \describe{
    \item{\code{signature(object = "modelorg")}}{
      method to use with objects of class \code{\linkS4class{modelorg}}.
    }
    \item{\code{signature(object = "summaryOptsol", mod = "modelorg")}}{
      method to use with objects of class \code{\linkS4class{summaryOptsol}}.
    }
  }
}

\arguments{
  \item{object}{
    An object of class \code{\linkS4class{modelorg}} or of class
    \code{\linkS4class{summaryOptsol}}.
  }
  \item{mod}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{react}{
    A numeric of character vector or an object of class
    \code{\linkS4class{reactId}} containing the reaction id's of reactions
    to print out.
  }
  \item{j}{
    A numeric of character vector indicating the simulations to consider,
    see Details.
    
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
  The output of the \code{\linkS4class{modelorg}} method is compatible to the
  file format produced by \code{\link{modelorg2tsv}}. Two columns are used:
  \code{"abbreviation"} containing the reaction id's and \code{"equation"}
  containing the reaction equation.
  
  The \code{\linkS4class{summaryOptsol}} method prints the limiting reactions
  generated in simulations and stored in objects of class
  \code{\linkS4class{summaryOptsol}}. Slot \code{react_id} of class
  \code{\linkS4class{summaryOptsol}} contains a list of reaction id's: list
  element \code{j} gives the reaction id's limiting simulation number \code{j}.
  
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\value{
  The \code{\linkS4class{modelorg}} method returns invisibly a character vector
  of length equal to the number of reactions given in argument \code{react}.
  Each string consists of two tab-delimited values: first, the reaction id,
  second, the reaction equation.
  
  The \code{\linkS4class{summaryOptsol}} returns invisibly a list of length
  equal to the number of elements in argument \code{j}. Each list element is
  of the same type as the return value of the \code{\linkS4class{modelorg}}
  method.
}

\seealso{
  Class \code{\linkS4class{modelorg}} and
  class \code{\linkS4class{summaryOptsol}}.
}

\keyword{methods}
