\name{onlyCheckGPR}
\alias{onlyCheckGPR}

\encoding{utf8}

\title{Check the GPR Rules}

\description{
  Checks the GPR Rules for the chosen reactions
}
\usage{
   onlyCheckGPR(model, gprRules, reactNr, verboseMode = 1)
}

\arguments{
  \item{model}{An object of class \code{\linkS4class{modelorg}}}
  \item{gprRules}{character: contains logical expressions.}
  \item{reactNr}{An object of class \code{\linkS4class{reactId}}, a
    numeric vector, or a character vector containing reaction id's.}
  \item{verboseMode}{integer: verbosity level.}
}


\details{
  The function checks the expressions for the chosen reactions.
}

\author{Benjamin Braasch}

\keyword{check}
