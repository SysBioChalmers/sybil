\name{onlyChangeGPR}
\alias{onlyChangeGPR}

\encoding{utf8}

\title{Change the GPR Rules}

\description{
  Changes the GPR Rules for the chosen reactions
}
\usage{
   onlyChangeGPR(model, gprRules, reactNr, verboseMode = 0)
}

\arguments{
  \item{model}{An object of class \code{\linkS4class{modelorg}}}
  \item{gprRules}{character: contains logical expressions.}
  \item{reactNr}{An object of class \code{\linkS4class{reactId}}, a
    numeric vector, or a character vector containing reaction id's.}
  \item{verboseMode}{integer: verbosity level.}
}


\details{
  The function changes the expressions for the chosen reactions.

  Use onlyCheckGPR first to check the expressions.
}

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\keyword{change}
