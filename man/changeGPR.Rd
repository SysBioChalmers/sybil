\name{changeGPR}
\alias{changeGPR}

\encoding{utf8}

\title{Check and Change the GPR Rules}

\description{
  Checks and Changes the GPR Rules for the chosen reactions
}
\usage{
   changeGPR(model, react, gprRules = "logicalExpression", verboseMode = 1)
}

\arguments{
  \item{model}{An object of class \code{\linkS4class{modelorg}}}
  \item{react}{An object of class \code{\linkS4class{reactId}}, a
    numeric vector, or a character vector containing reaction id's.}
  \item{gprRules}{character: contains logical expressions.}
  \item{verboseMode}{integer: verbosity level.}
}


\details{
  The function changes the expressions for the chosen reactions.

  The function stops if any logic expressions is not correct.
  Then the changes are executed.
}

\author{Benjamin Braasch}

