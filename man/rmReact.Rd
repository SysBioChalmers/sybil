\name{rmReact}
\alias{rmReact}

\encoding{utf8}

\title{Remove Reactions From a Model}

\description{
  The function \code{rmReact} removes reactions from a model.
}

\usage{
  rmReact(model, react, rm_met = TRUE)
}

\arguments{
  \item{model}{An object of class \code{\linkS4class{modelorg}}}
  \item{react}{An object of class \code{\linkS4class{reactId}}, a
    numeric vector, or a character vector containing reaction id's.}
  \item{rm_met}{Logical: also remove unused metabolites (default: TRUE).}
}

\details{
  The argument \code{react} is evaluated by the function
  \code{\link{checkReactId}}. 
}

\value{
  An object of class \code{\linkS4class{modelorg}}.  
}

\references{
  Becker, S. A., Feist, A. M., Mo, M. L., Hannum, G., Palsson, B. Ø.  and
  Herrgard, M. J. (2007) Quantitative prediction of cellular metabolism with
  constraint-based models: the COBRA Toolbox. \emph{Nat Protoc} \bold{2},
  727--738.

  Schellenberger, J., Que, R., Fleming, R. M. T., Thiele, I., Orth, J. D.,
  Feist, A. M., Zielinski, D. C., Bordbar, A., Lewis, N. E., Rahmanian, S.,
  Kang, J., Hyduke, D. R. and Palsson, B. Ø. (2011) Quantitative prediction of
  cellular metabolism with constraint-based models: the COBRA Toolbox v2.0.
  \emph{Nat Protoc} \bold{6}, 1290--1307.
}

\author{Gabriel Gelius-Dietrich}

\seealso{
  \code{\linkS4class{modelorg}},
  \code{\linkS4class{reactId}} and \code{\link{checkReactId}}
}

\examples{
  data(Ec_core)
  Ec_r <- rmReact(Ec_core, c("ATPM", "Biomass"))
}

\keyword{manip}

