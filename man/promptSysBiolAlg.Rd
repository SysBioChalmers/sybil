\name{promptSysBiolAlg}
\alias{promptSysBiolAlg}

\title{
  Generate A Skeletal Structure of Subclasses of sysBiolAlg
}

\description{
  Generates a skeletal structure of new subclasses of class
  \code{\linkS4class{sysBiolAlg}}, in particular for the
  constructor method \code{\link{initialize}}.
}

\usage{
  promptSysBiolAlg(algorithm, prefix = "sysBiolAlg", sep = "_",
                   suffix = "R", fpath = ".", ...)
}

\arguments{
  \item{algorithm}{
    A single character string containing the name of the new algorithm.
  }
  \item{prefix}{
    A single character string containing a prefix for the new algorithm, see
    Details below.\cr
    Default: \code{"sysBiolAlg"}.
  }
  \item{sep}{
    A single character string containing a separator for \code{prefix} and
    \code{algorithm}.\cr
    Default: \code{"_"}.
  }
  \item{suffix}{
    A single character string containing a file name suffix.\cr
    Default: \code{"R"}.
  }
  \item{fpath}{
    A single character string containing a file path.\cr
    Default: \code{\dots}.
  }
  \item{\dots}{
    Further arguments passed to \code{\link{file}}.
  }
}

\details{
  The arguments \code{prefix} \code{algorithm} are stick together separated by
  \code{sep} (default: a single underscore \code{"_"}) to get the new class
  name: \code{prefix_algorithm}. The filename will be:
  \code{prefix_algorithmClass.R}.
  
  The class definition in the new file will extend class
  \code{\linkS4class{sysBiolAlg}} directly and will not add any
  slots. Additionally a skeletal structure for method \code{\link{initialize}}
  will be generated. In this method, the user should create all arguments to
  the \code{initialize} method described in the base class
  \code{\linkS4class{sysBiolAlg}} and put them all to
  \code{\link{callNextMethod}}. Or, alternatively, generate an instance of
  class \code{\linkS4class{optObj}} \dQuote{by hand}.
}

\value{
  Returns \code{NULL} invisible.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  \code{\linkS4class{sysBiolAlg}}
}

\keyword{ IO }
