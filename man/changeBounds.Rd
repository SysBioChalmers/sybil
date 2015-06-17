\name{changeBounds}
\alias{changeBounds}

\encoding{utf8}

\title{
  Change Variable Bounds in a Metabolic Network
}

\description{
  The function changes the upper and/or lower bounds of a given metabolic
  network model to new values.
}

\usage{
  changeBounds(model, react, lb = NULL, ub = NULL)
}

\arguments{
  \item{model}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{react}{
    An object of class \code{\linkS4class{reactId}}, character or integer.
    Specifies the fluxes (variables) for which to change the upper and/or lower
    bounds.
  }
  \item{lb}{
    Numeric vector giving the lower bounds for the fluxes mentioned in
    \code{react}. If missing, lower bounds are set to zero. If \code{lb} has
    a length of 1, the value of \code{lb} will be used for all reactions in
    \code{react}.
  }
  \item{ub}{
    Numeric vector giving the upper bounds for the fluxes mentioned in
    \code{react}. If missing, upper bounds are set to zero. If \code{ub} has
    a length of 1, the value of \code{ub} will be used for all reactions in
    \code{react}.
  }
}

\details{
  The argument \code{react} will be evaluated by the function
  \code{\link{checkReactId}}.
}

\value{
  Returns the given model (an object of the same class as the argument
  \code{lpmodel}) containing the new objective function.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{\code{\link{checkReactId}}}

\examples{
  ## change the E.coli core model to lactate input:
  data(Ec_core)
  Ec_new <- changeBounds(Ec_core,
                         c("EX_glc", "EX_lac"),
                         lb = c(0, -20), ub = 1000)
}

\keyword{optimize}

