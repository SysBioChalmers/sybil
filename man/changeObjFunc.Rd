\name{changeObjFunc}

\alias{changeObjFunc}

\encoding{utf8}

\title{Sets/changes the Objective Function}

\description{
  The function \code{changeObjFunc} changes or sets the objective
  function for a specified model.
}

\usage{
  changeObjFunc(model, react, obj_coef = rep(1, length(react)))
}

\arguments{
  \item{model}{
    An object of class \code{\linkS4class{modelorg}}.
  }
  \item{react}{
    An object of class \code{\linkS4class{reactId}}, character or integer.
    Specifies the fluxes (variables) for which to change the objective
    coefficients.
  }
  \item{obj_coef}{
    A numerical vector with length equal to the number of reaction id's
    given in argument \code{react} containing the objective coefficients.\cr
    Default: a value of one for each reaction given in argument \code{react}.
  }
}

\details{
  The argument \code{react} will be evaluated by the function
  \code{\link{checkReactId}}. The return value is used to change
  the objective function.
  
  All reactions not given in argument \code{react} will get an objective
  value of zero.
}

\value{
  Returns the given model containing the new objective function.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{\code{\link{checkReactId}}}

\examples{
  ## sets the objective function to the ATP maintenance reaction:
  data(Ec_core)
  Ec_new <- changeObjFunc(Ec_core, "ATPM")
}

\keyword{optimize}
