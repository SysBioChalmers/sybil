\name{addReact}
\alias{addReact}

\encoding{utf8}

\title{
  Add/Change Reactions in a Model
}

\description{
  The function \code{addReact} adds one reaction to a metabolic model, or
  changes one reaction in a metabolic model.
}

\usage{
  addReact(model,
           id,
           met,
           Scoef,
           reversible = FALSE,
           lb = 0,
           ub = SYBIL_SETTINGS("MAXIMUM"),
           obj = 0,
           subSystem = NA,
           gprAssoc = NA,
           reactName = NA,
           metName = NA,
           metComp = NA)
}

\arguments{
  \item{model}{
    An object of class \code{modelorg}.
  }
  \item{id}{
    A single character string containing a reaction id (see details below).
  }
  \item{met}{
    A vector of character strings containing the metabolite id's used in the
    reaction given in \code{Scoef}.
  }
  \item{Scoef}{
    A numeric vector of the same length as \code{met} of stoichiometric
    coefficients for the metabolites in \code{met}. The value in \code{Scoef[i]}
    is the stoichiometric coefficient of the metabolite in \code{met[i]}.
  }
  \item{reversible}{
    A Boolean value, indicating if the reaction is reversible or not.\cr
    Default: \code{FALSE}.
  }
  \item{lb}{
    A single numeric value giving the lower bound of the reaction.\cr
    Default: \code{0}.
  }
  \item{ub}{
    A single numeric value giving the upper bound of the reaction.\cr
    Default: \code{SYBIL_SETTINGS("MAXIMUM")}.
  }
  \item{obj}{
    A single numeric value giving the objective coefficient of the reaction.\cr
    Default: \code{0}.
  }
  \item{subSystem}{
    A vector of character strings containing the sub systems to which the
    reaction belongs. All values must be available in
    \code{\link{subSys}(model)}. If \code{NA}, the reaction will not be
    associated to any sub system.\cr
    Default: \code{NA}.
  }
  \item{gprAssoc}{
    A single character string giving the gpr association for the reaction. If
    \code{NA}, no gpr association is created.\cr
    Default: \code{NA}.
  }
  \item{reactName}{
    A single character string giving the name for the reaction. If \code{NA},
    the value of argument \code{id} is used.\cr
    Default: \code{NA}.
  }
  \item{metName}{
    A vector of character strings of the same length as \code{met} containing
    the the metabolites names for the metabolites given in argument \code{met}.
    If set to \code{NA}, the metabolite id's are used.
    Default: \code{NA}.
  }
  \item{metComp}{
    A vector of character strings or integers of the same length as \code{met}
    containing a compartment name (as in \code{\link{mod_compart}(model)}) or an
    index pointing to a value in \code{\link{mod_compart}(model)} (as in
    \code{met_comp(model)}). If \code{NA}, the metabolites will not be
    associated to any compartment.\cr
    Default: \code{NA}.
  }
}

\details{
  The function \code{addReact} can be used to add reactions and/or metabolites
  to a given metabolic model, or to change parameters of a reaction already
  present in a given metabolic model.
  If the reaction id in argument \code{id}is already present in the given model,
  this reaction will be changed, no new column will be added to the
  stoichiometric matrix. If any of the metabolite id's of argument \code{met}
  are not present in the model, they will be added (new rows in the
  stoichiometric matrix will be added).

  Arguments \code{subSystem}, \code{gprAssoc} and \code{reactName} are only
  used, if a new reaction is added to the model (if \code{id} is not in
  \code{\link{react_id}(model)}, exact matching is used).
}

\value{
  An object of class \code{\linkS4class{modelorg}}, or
  \code{\linkS4class{modelorg_irrev}}, if \code{model} is of class
  \code{\linkS4class{modelorg_irrev}}. 
}

\examples{
data(Ec_core)

# add reaction A + 2 B <-> C to the model
modelNew <- addReact(Ec_core, id="newReact", met=c("A", "B", "C"), Scoef=c(-1, -2, 1), reversible=TRUE, lb=-1000, ub=1000, obj=0)

# view the new reaction
shrinkMatrix(modelNew, j="newReact")
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

\author{
  Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>

  Maintainer: Claus Jonathan Fritzemeier <clausjonathan.fritzemeier@uni-duesseldorf.de>
}


\seealso{
  \code{\linkS4class{modelorg}} and \code{\link{rmReact}}
}

\keyword{manip}

