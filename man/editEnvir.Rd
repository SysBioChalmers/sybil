\name{editEnvir}
\alias{editEnvir}

\title{
  Environment Editor for Metabolic Networks
}
\description{
  Environment editor for metabolic networks. The function \code{editEnvir}
  opens the exchange reactions of a metabolic network in R's data editor.
  Changes in upper and lower bounds will be set in the given model.
}
\usage{
  editEnvir(model, newKey = FALSE, ...)
}

\arguments{
  \item{model}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{newKey}{
    If set to \code{TRUE}, a new model key will be generated.
  }
  \item{\dots}{
    Further arguments passed to \code{\link{edit}}.
  }
}

\value{
  An object of class \code{\linkS4class{modelorg}}.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{\code{\link{checkReactId}}}

\examples{
  \dontrun{
  ## change environment of E.coli core model:
  data(Ec_core)
  mod <- editEnvir(Ec_core)
  }
}


